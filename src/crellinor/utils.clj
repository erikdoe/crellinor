(ns crellinor.utils
  (:import (java.util SplittableRandom)))

(defn sq [x]
  (* x x))

(defn log
  ([line] (log "*" line))
  ([prefix line] (println (str prefix " " line))))


(defonce rng (new SplittableRandom))

(defn set-srand-seed! [seed]
  (def rng (new SplittableRandom seed)))

(defn srand []
  (.nextDouble rng))

(defn srand-int [n]
  (.nextInt rng n))


(def fake-rand-vals (atom [1 2 3]))
(def fake-rand-idx (atom 0))

(defn set-fake-rand-vals! [v]
  (reset! fake-rand-idx 0)
  (reset! fake-rand-vals v))

(defn fake-rand-int [n]
  (let [v (nth @fake-rand-vals @fake-rand-idx)]
    (swap! fake-rand-idx #(mod (inc %) (count @fake-rand-vals)))
    v))
