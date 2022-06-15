(ns crellinor.processor
  (:require [clojure.core.strint :refer [<<]]
            [crellinor.parameters :refer [get-param]]
            [crellinor.utils :refer [log]]
            [crellinor.creatures :as c]
            [crellinor.terrain :as t]
            [crellinor.parameters :as params]
            [crellinor.gp :as gp]))


; -- statistics

(def instr-counters (zipmap (keys (params/get-param :instructions)) (repeatedly #(atom 0))))

(defn reset-instr-counts! []
  (dorun (map #(reset! %1 0) (vals instr-counters))))

(defn incr-instr-count! [instr]
  (if-let [counter (get instr-counters instr)]
    (swap! counter inc))
  )


; -- Turning and moving

(defn add-to-bearing [creature delta]
  (assoc creature :bearing (mod (+ (:bearing creature) delta) 360)))

(defn instr-turn-left [creature]
  (add-to-bearing creature 270))

(defn instr-turn-right [creature]
  (add-to-bearing creature 90))

(defn instr-move [creature]
  (let [desired-pos (t/pos-ahead (:pos creature) (:bearing creature))]
    (if-let [other-creature (t/creature-at desired-pos)]
      (do
        (gp/mate creature other-creature (:lastprocd creature))
        (add-to-bearing creature 90))
      (do
        ;(log "#" (<< "move! ~{(:id creature)} moves from ~{(:pos creature)} to ~{desired-pos}"))
        (t/move-creature! (dissoc creature :pos) (:pos creature) desired-pos)
        (assoc creature :pos desired-pos)))))


; -- Eating

(defn instr-eat [creature]
  (if-let [plant (t/plant-at (:pos creature))]
    (let [ep-consumable (min
                          (inc (- (get-param :creature-max-ep) (:ep creature)))
                          (get-param :eat-ep))]
      (if (> ep-consumable (:ep plant))
        (do
          (t/remove-plant-at! (:pos creature))
          (assoc creature :ep (+ (:ep creature) (:ep plant))))
        (do
          (t/upsert-plant-at! (:pos creature) (assoc plant :ep (- (:ep plant) ep-consumable)))
          (assoc creature :ep (+ (:ep creature) ep-consumable)))))
    creature))


; -- Jumps

(defn instr-jump-section [creature]
  (c/inc-pc-to-section creature))

(defn instr-jump-this-section [creature]
  (c/dec-pc-to-section creature))

(defn instr-jump-zero [creature]
  (c/set-pc creature 0))

(defn instr-jump-rel [creature n]
  (c/inc-pc creature (dec n)))


; -- Branches

(defn instr-branch-on-cflag [creature pred]
  (if (pred (:cflag creature))
    (c/inc-pc-to-section creature)
    creature))


; -- Checks

(defn instr-check-food-here [creature]
  (assoc creature :cflag (some? (t/plant-at (:pos creature)))))

(defn instr-check-food-ahead [creature]
  (let [positions (t/pos-ahead (:pos creature) (:bearing creature) (get-param :view-distance))]
    (assoc creature :cflag (reduce #(if (t/plant-at %2) true %1) false positions))))


; -- Clear

(defn instr-clear-check-flag [creature]
  (assoc creature :cflag false))


; -- Combined branch and check

(defn instr-branch-if-food-here [creature]
  (if (some? (t/plant-at (:pos creature)))
    (c/inc-pc-to-section creature)
    creature)
  )

(defn instr-branch-unless-food-here [creature]
  (if (some? (t/plant-at (:pos creature)))
    creature
    (c/inc-pc-to-section creature))
  )

(defn instr-branch-if-food-ahead [creature]
  (let [positions (t/pos-ahead (:pos creature) (:bearing creature) (get-param :view-distance))]
    (if (reduce #(if (t/plant-at %2) true %1) false positions)
      (c/inc-pc-to-section creature)
      creature))
  )

; -- Miscellaneous

(defn instr-nop [creature]
  creature)


; -- dispatch table

(def instr-functions
  {:MOV instr-move
   :TUL instr-turn-left
   :TUR instr-turn-right
   :EAT instr-eat
   :JMS instr-jump-section
   :JTS instr-jump-this-section
   :JMZ instr-jump-zero
   :JR2 #(instr-jump-rel %1 2)
   :JR4 #(instr-jump-rel %1 4)
   :BTR #(instr-branch-on-cflag %1 true?)
   :BNT #(instr-branch-on-cflag %1 false?)
   :CLC instr-clear-check-flag
   :CFH instr-check-food-here
   :CFA instr-check-food-ahead
   :BFH instr-branch-if-food-here
   :BFA instr-branch-if-food-ahead
   :NOP instr-nop
   }
  )


; -- core processing loop

(defn- fetch-instr [creature]
  (let [instr (c/current-instr creature)
        cycles (get (get-param :instructions) instr 1)]
    (assoc creature :cc cycles))
  )

(defn- fetch-instr-if-neccessary [creature]
  (if (= 0 (:cc creature))
    (fetch-instr creature)
    creature))

(defn- exec-instr [creature]
  (let [instr (c/current-instr creature)
        with-inc-pc (c/inc-pc creature 1)]
    (incr-instr-count! instr)
    (if-let [instr-fn (get instr-functions instr)]
      (apply instr-fn [with-inc-pc])
      (throw (Exception. (str "Unknown instruction; found" instr))))))

(defn- exec-instr-if-ready [creature]
  (if (= 0 (:cc creature))
    (exec-instr creature)
    creature))

(defn- update-world! [creature]
  (if (and (> (:ep creature) 0)
           (< (c/age creature) (get-param :max-age)))
    (t/upsert-creature-at! (:pos creature) (dissoc creature :pos))
    (t/remove-creature-at! (:pos creature)))
  )

(defn do-cycle! [creature t pos]
  (dosync
    (-> creature
        (assoc :lastprocd t)
        (assoc :pos pos)
        (fetch-instr-if-neccessary)
        (c/dec-attr :cc)
        (exec-instr-if-ready)
        (c/dec-attr :ep)
        (update-world!))
    ))


; -- helper for functional tests

(defn cycle-count [instructions]
  (apply + (map #(get (get-param :instructions) % 1) instructions)))


