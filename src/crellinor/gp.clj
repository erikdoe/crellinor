(ns crellinor.gp
  (:require [clojure.set :refer :all]
            [crellinor.utils :refer [srand-int]]
            [crellinor.parameters :refer [get-param]]
            [crellinor.terrain :as t]
            [crellinor.creatures :as c]))


; -- functions for creatures' programs

(defn rand-instr [num]
  (let [instr-list (keys (get-param :instructions))
        n (count instr-list)
        rand-seq (repeatedly #(srand-int n))]
    (map #(nth instr-list %) (take num rand-seq))))

(defn rand-program []
  (vec (rand-instr (get-param :program-length))))


; -- main mate function

(defn- can-mate? [parent0 parent1 t]
  (and (>= (:ep parent0) (get-param :min-mating-ep))
       (c/mature? parent0)
       (c/mature? parent1)))

(defn- random-single-point-crossover [prog0 prog1]
  (let [l (get-param :program-length)
        p (srand-int l)]
    (into (take p prog0) (take-last (- l p) prog1))
    ))

(defn- choose [p0 p1]
  (if (= 0 (srand-int 2))
    p0
    p1))

(defn uniform-crossover [prog0 prog1]
  (let [slen (get-param :xsection-length)
        p0 (partition slen prog0)
        p1 (partition slen prog1)]
    (vec (flatten (map choose p0 p1)))))


(defn cut-n-splice [prog0 prog1]
  (let [len (srand-int (dec (count prog1)))
        si (srand-int (- (count prog1) len))
        di (srand-int (- (count prog0) len))]
    (vec
      (concat
        (subvec prog0 0 di)
        (subvec prog1 si (+ si len))
        (subvec prog0 (+ di len) (count prog0))))))


(defn mate [parent0 parent1 t]
  (if (can-mate? parent0 parent1 t)
    (let [pos (t/free-pos-near (:pos parent0))]
      (if (some? pos)
        (let [new-prog (cut-n-splice (:program parent0) (:program parent1))
              child (merge (c/add-birth-data (c/new-creature new-prog) pos t)
                           {:p0      (:id parent0)
                            :p1      (:id parent1)
                            :bearing (nth [0 90 180 270] (srand-int 4))})]
          (t/upsert-creature-at! pos child)
          (merge parent0 {:ep (- (:ep parent0) (get-param :creature-start-ep))}))
        parent0))
    parent0))


; -- program analysis

(defn can-move-and-eat? [creature]
  (= 2 (count (intersection #{:EAT :MOV} (set (:program creature)))))
  )

