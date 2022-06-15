(ns crellinor.core
  (:require clojure.pprint
            [clojure.core.strint :refer [<<]]
            [cheshire.core :refer :all]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [crellinor.parameters :refer [get-param]]
            [crellinor.terrain :as t]
            [crellinor.creatures :as c]
            [crellinor.world :as w]
            [crellinor.web :as web]
            [crellinor.processor :as proc]
            )
  (:gen-class))

; -- repl helper

(defn mw []
  (w/reset-world!)
  (w/add-initial-creatures-and-plants!)
  )

(defn pw []
  (println (t/ascii-map)))

(defn pw-json []
  (print (generate-string (map #(:c0 (deref %)) (deref t/locations-with-creature)) {:pretty true})))

(defn rp []
  (mw)
  (tufte/add-basic-println-handler! {})
  (profile {:dynamic? true} (w/do-cycles-log-timing! 50)))


(def patroller
  (c/new-creature [:TUR :MOV :EAT :MOV :EAT
                   :MOV :EAT :MOV :EAT]))

(def lawn-mower
  (c/new-creature [:TUR :MOV :EAT :TUR :MOV :EAT
                   :TUL :MOV :EAT :TUL :MOV :EAT]))

(def eat-and-turn
  (c/new-creature [:CFH :BTR :MOV :JMZ :NOP
                   :EAT :TUR :MOV]))

;(add-creature! patroller [10 9])
;(add-creature! lawn-mower [2 2])
;(add-creature! eat-and-turn [5 5])





