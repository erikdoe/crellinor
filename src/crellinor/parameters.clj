(ns crellinor.parameters
  (:require [clojure.java.io :as io]
            [clojure.walk :refer [keywordize-keys]]
            [cheshire.core :refer [parse-stream]]
            ))


(def default-parameters
  {:world-end         1000000

   :world-size        150
   :start-pop-size    500
   :start-plant-count 5000

   :plant-start-ep    1000
   :plant-prob        1/4

   :creature-start-ep 500
   :creature-max-ep   5000
   :max-age           100000

   :eat-ep            200
   :view-distance     10
   :min-mating-ep     4000

   :program-length    16
   :psection-length   4
   :xsection-length   4

   :instructions      {
                       :EAT 10 :MOV 5
                       :TUL 3 :TUR 3
                       :JMS 1 :JTS 1 :JMZ 1 :JR2 1 :JR4 1
                       :CLC 1 :CFH 1 :CFA 1 :BTR 1 :BNT 1
                       :BFH 1 :BFA 1
                       :NOP 1
                       }
   })


(def parameters
  (atom default-parameters))

(defn load-params [filename]
  (let [params-from-file (keywordize-keys (parse-stream (io/reader filename)))]
    (swap! parameters #(into %1 params-from-file))))

(defn set-param! [param-name value]
  (swap! parameters #(assoc %1 param-name value)))

(defn reset-param! [param-name]
  (swap! parameters #(assoc %1 param-name (get default-parameters param-name))))

(defn get-param [param-name]
  (get @parameters param-name))
