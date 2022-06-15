(ns crellinor.statistics
  (:require [taoensso.tufte :refer (defnp p profiled profile)]
            [crellinor.creatures :as c]
            [crellinor.terrain :as t]
            [crellinor.processor :as proc]))

(def statistics
  (ref
    {
     :population-series   []
     :energy-level-series []
     :instr-count-series  []
     }))

(defn reset-statistics! []
  (dosync
    (alter statistics #(assoc % :population-series []))
    (alter statistics #(assoc % :energy-level-series []))
    (alter statistics #(assoc % :instr-count-series []))))


(defn current-population []
  (count (deref t/locations-with-creature)))

(defn push-population! []
  (dosync
    (alter statistics #(assoc % :population-series (conj (:population-series %) (current-population))))))

(defn population-series []
  (:population-series (deref statistics)))


(defn current-energy-levels
  ([fn]
    (->> t/terrain
         (map #(apply fn [(deref %)]))
         (filter some?)
         (map #(get % :ep))
         (reduce +)
        ))
  ([]
    [(current-energy-levels #(:plant %))
     (current-energy-levels #(if (and (some? (:c0 %)) (c/mature? (:c0 %))) (:c0 %)))
     (current-energy-levels #(if (and (some? (:c0 %)) (not (c/mature? (:c0 %)))) (:c0 %)))]))

(defn push-energy-levels! []
  (dosync
    (alter statistics #(assoc % :energy-level-series (conj (:energy-level-series %) (current-energy-levels))))))

(defn energy-level-series []
  (:energy-level-series (deref statistics)))


(defn update-values [m f & args]
  (into {} (for [[k v] m] [k (apply f v args)])))

(defn current-instr-counts []
  (let [total (apply + (map deref (vals proc/instr-counters)))]
    (if (= 0 total)
      (update-values proc/instr-counters #(deref %1))
      (update-values proc/instr-counters #(/ (deref %1) total)))))

(defn push-instr-counts! []
  (dosync
    (alter statistics #(assoc % :instr-count-series (conj (:instr-count-series %) (current-instr-counts)))))
  (proc/reset-instr-counts!))

(defn instr-count-series []
  (:instr-count-series (deref statistics)))
