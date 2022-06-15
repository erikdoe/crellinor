(ns crellinor.web
  (:require clojure.pprint
            [clojure.core.strint :refer [<<]]
            [clojure.java.io :as io]
            [ring.util.http-response :as response]
            [ring.middleware.file :refer [wrap-file]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [compojure.api.sweet :refer [api context GET]]
            [compojure.api.sweet :refer :all]
            [compojure.route :as route]
            [crellinor.parameters :refer [get-param]]
            [crellinor.utils :refer [log sq set-srand-seed!]]
            [crellinor.world :as w]
            [crellinor.terrain :as t]
            [crellinor.statistics :as stats]
            [crellinor.creatures :as c]
            [crellinor.parameters :as params]))


; -- initialization

(defn start []
  ; called via lein-ring
  (log "Welcome to Crellinor")
  (params/load-params (io/resource "worldfile.json"))
  (if-let [seed (get-param :seed)]
    (set-srand-seed! seed))
  (let [energy-input (* (get-param :plant-prob) (get-param :plant-start-ep))
        locs-per-creature (int (/ (sq (get-param :world-size)) energy-input))
        max-distance (/ (get-param :creature-max-ep) (get (get-param :instructions) :MOV 1))
        plants-til-full (/ (get-param :creature-max-ep) (get-param :plant-start-ep))]
    (log (<< "- Seed: ~{(get-param :seed)}"))
    (log (<< "- Energy input per cycle: ~{energy-input} EP"))
    (log (<< "- Creatures to locations: 1:~{locs-per-creature}"))
    (log (<< "- Max distance without eating: ~{max-distance}"))
    (log (<< "- Plants to fully replenish creature energy: ~{plants-til-full}")))
  (log "Populating world")
  (w/reset-world!)
  (w/add-initial-creatures-and-plants!)
  (log "Starting simulation")
  (.start (Thread. w/do-cycles-until-end!))
  )


; -- data endpoints

(defn creature-data-for-map [creature]
  (let [[x y] (:pos creature)
        t (w/get-time)]
        {:id      (:id creature)
         :p0      (:p0 creature)
         :p1      (:p1 creature)
         :x       x
         :y       y
         :b       (:bearing creature)
         :adult   (if (c/mature? creature) 1 0)
         :ep      (:ep creature)
         :pc      (:pc creature)
         :program (c/format-program (:program creature))
         }))

(defn plant-data-for-map [plant]
  (let [[x y] (:pos plant)]
    {
     :x  x
     :y  y
     :ep (:ep plant)
     }
    ))

(defn worldmap-data []
  {:worldSize (get-param :world-size)
   :initialEP (get-param :creature-start-ep)
   :plantEP   (get-param :plant-start-ep)
   :plants    (map plant-data-for-map (t/all-plants))
   :creatures (map creature-data-for-map ((dosync t/all-creatures-from-terrain)))}
  )

(defn creature-list-data []
  (t/all-creatures))


; -- request handler wrappers

(defn wrap-ui [handler ui-dir]
  (wrap-file handler ui-dir))

(defn wrap-no-cache [handler]
  (fn [request]
    (if-let [response (handler request)]
      (assoc-in response [:headers "Cache-Control"] "no-cache")
      (handler request))))


; -- routes

(def app
  ; called via lein-ring
  (api
    (context "/ui" []
      (-> (route/not-found "File Not Found")
          (wrap-ui "resources/ui")
          (wrap-content-type)
          (wrap-no-cache)
          ))

    (context "/data" []
      (GET "/worldmap" {params :params}
        (if (:cycles params)
          (w/do-cycles-log-timing! (read-string (:cycles params))))
        (response/ok (worldmap-data)))
      (GET "/creatures" []
        (response/ok (creature-list-data)))
      )

    (context "/stats" []
      (GET "/population" []
        (response/ok (map-indexed #(conj [(* 500 %1)] %2) (stats/population-series))))
      (GET "/energy-level" []
        (response/ok (map-indexed #(into [(* 500 %1)] %2) (stats/energy-level-series))))
      (GET "/instr-counts" []
        (response/ok (map-indexed #(conj [(* 500 %1)] %2) (stats/instr-count-series))))
      )

    (context "/" []
      (GET "/" []
        (response/temporary-redirect "/ui/index.html"))
      )

    (undocumented
      (route/not-found "404 not found"))

    ))

