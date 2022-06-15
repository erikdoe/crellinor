(ns crellinor.world
  (:require [clojure.core.strint :refer [<<]]
            [taoensso.tufte :refer (defnp p profiled profile)]
            [crellinor.utils :refer [log sq srand]]
            [crellinor.statistics :as stats]
            [crellinor.parameters :refer [get-param]]
            [crellinor.creatures :as c]
            [crellinor.terrain :as t]
            [crellinor.gp :as gp]
            [crellinor.processor :as proc]))


; -- world state

(def worldtime (atom 0))

(defn inc-worldtime! []
  (swap! worldtime inc))

(defn get-time []
  @worldtime)

; -- helper

(defn add-creature!
  ([creature pos]
   (dosync
     (t/upsert-creature-at! pos (-> creature
                                    (c/add-birth-data pos @worldtime)
                                    (assoc :mcycle (get-param :creature-start-ep))))))
  ([creature]
   (add-creature! creature (t/rand-pos)))
  ([]
   (add-creature! (c/new-creature (gp/rand-program)))))

(defn add-plant!
  ([plant pos]
   (dosync
     (t/upsert-plant-at! pos plant)))
  ([]
   (add-plant! (c/new-plant) (t/rand-pos))))


; -- set up initial world

(defn reset-world! []
  (dosync
    (reset! worldtime 0)
    (t/reset-terrain!)
    ))

(defn rand-creatures [n]
  (->> (repeatedly #(c/new-creature (gp/rand-program)))
       (filter gp/can-move-and-eat?)
       (take n)
       (doall))
  )

(defn add-initial-creatures-and-plants! []
  (let [start-population (rand-creatures (get-param :start-pop-size))]
    (dorun (map add-creature! start-population))
    (dorun (repeatedly (get-param :start-plant-count) add-plant!)))
  )


; -- running one cycle of the simulation

(defn- grow-plants! []
  (if (< (srand) (* (get-param :plant-prob) (min 1 (/ 200000 @worldtime))))
    (add-plant!)))

(defn- run-program! [lref t]
  (if-let [creature (:c0 (deref lref))]
    (if (= (:lastprocd creature) (dec t))
      (proc/do-cycle! creature t (:pos (deref lref)))))
  nil)

(defn- run-all-programs! []
  (let [t @worldtime]
    ; because we run single-threaded we put all processing in one tx
    (dosync
      (dorun
        (map #(run-program! % t) (deref t/locations-with-creature))))))

(defn do-one-cycle! []
  (p ::do-one-cycle!
     (inc-worldtime!)
     (grow-plants!)
     (run-all-programs!)))


; -- running multiple cycles

(defn do-cycles! [n]
  (dorun (repeatedly n #(do-one-cycle!))))

(defn do-cycles-log-timing! [n]
  (let [start (System/currentTimeMillis)]
    (do-cycles! n)
    (let [population (last (stats/population-series))
          millis (- (System/currentTimeMillis) start)
          cpm (int (/ (* n population) millis))
          timestr (if (< millis 10000) (str millis "ms") (str (quot millis 1000) "s"))]
      (log (<< "~{@worldtime}: Ran ~{n} cycles for ~{population} creatures in ~{timestr} (~{cpm} cycles/ms)"))
      )))

(defn do-cycles-until-end! []
  (log (<< "Will run for ~{(get-param :world-end)} cycles."))
  (stats/push-population!)
  (stats/push-energy-levels!)
  (stats/push-instr-counts!)
  (doseq [_ (range (/ (get-param :world-end) 10))]
    (do-cycles-log-timing! 10)
    (stats/push-population!)
    (stats/push-energy-levels!)
    (stats/push-instr-counts!)
    )
  )
