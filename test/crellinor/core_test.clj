(ns crellinor.core-test
  (:require [clojure.test :refer :all]
            [crellinor.core :refer :all]
            [crellinor.parameters :refer [get-param set-param! reset-param!]]
            [crellinor.processor :as p]
            [crellinor.creatures :as c]
            [crellinor.terrain :as t]
            [crellinor.world :as w]
            ))


(defn reset-world-fixture [f]
  (w/reset-world!)
  (f))

(use-fixtures :each reset-world-fixture)


; -- adding and removing creatures and plants

(deftest add-creature
  (doall (repeatedly 8 #(w/inc-worldtime!)))
  (w/add-creature! (c/new-creature []) [1 2])
  (let [bob (t/creature-at [1 2])]
    (is (some? bob))
    (is (= "8-1-2" (:id bob)))
    (is (= 8 (:bcycle bob)))
    ))

(deftest add-plant
  (w/add-plant! (c/new-plant) [1 2])
  (let [plant (t/plant-at [1 2])]
    (is (some? plant))
    ))

(deftest remove-plant
  (w/add-plant! (c/new-plant) [1 2])
  (dosync (t/remove-plant-at! [1 2]))
  (let [plant (t/plant-at [1 2])]
    (is (nil? plant))
    ))


; -- instruction processing

(deftest proc-increments-pc-and-updates-lastprocd
  (doall (repeatedly 8 #(w/inc-worldtime!)))
  (w/add-creature! (c/new-creature [:NOP :NOP]) [0 0])
  (w/do-one-cycle!)
  (let [bob (t/creature-at [0 0])]
    (is (= 1 (:pc bob)))
    (is (= 9 (:lastprocd bob)))
    ))

(deftest proc-restarts-program-at-end
  (w/add-creature! (c/new-creature [:NOP :NOP]) [0 0])
  (w/do-cycles! 3)
  (let [bob (t/creature-at [0 0])]
    (is (= 1 (:pc bob)))
    ))

(deftest proc-instr-is-run-when-all-its-cycles-passed
  (w/add-creature! (c/new-creature [:TUR :NOP :NOP :NOP :NOP :NOP]) [0 0])
  (w/do-cycles! 1)
  (testing "fetch only affects cycle counter"
    (let [bob (t/creature-at [0 0])]
      (is (= 0 (:pc bob)))
      (is (< 0 (:cc bob)))
      (is (= 0 (:bearing bob)))
      ))
  (w/do-cycles! (dec (p/cycle-count [:TUR])))
  (testing "exec runs behaviour and increments pc"
    (let [bob (t/creature-at [0 0])]
      (is (= 1 (:pc bob)))
      (is (= 0 (:cc bob)))
      (is (= 90 (:bearing bob)))
      )))

(deftest proc-each-creature-once-per-cycle
  ; when a creature moves to a new loc that wasn't processed yet
  ; its program shouldn't be run again on the new loc
  (w/add-creature! (assoc (c/new-creature [:MOV :CFH :NOP]) :bearing 90) [0 0])
  (w/do-cycles! (p/cycle-count [:MOV]))
  (let [bob (t/creature-at [1 0])]
    (is (= 1 (:pc bob)))
    ))


; -- max age

(deftest proc-removes-creature-when-it-reaches-max-age
  (set-param! :max-age 10)
  (w/add-creature! (c/new-creature [:NOP]) [1 2])
  (w/do-cycles! (dec (get-param :max-age)))
  (is (some? (t/creature-at [1 2])))
  (w/do-cycles! 1)
  (is (nil? (t/creature-at [1 2])))
  (reset-param! :max-age)
  )


; -- movement

(deftest turn-right-adds-90deg-to-bearing
  (let [bob (c/new-creature [:TUR])]
    (w/add-creature! bob [1 2])
    (w/do-cycles! (p/cycle-count (:program bob)))
    (is (= 90 (:bearing (t/creature-at [1 2]))))))

(deftest turn-left-subtracts-90deg-from-bearing-witch-stays-0-360
  (let [bob (c/new-creature [:TUL])]
    (w/add-creature! bob [1 2])
    (w/do-cycles! (p/cycle-count (:program bob)))
    (is (= 270 (:bearing (t/creature-at [1 2]))))))

(deftest turn-left-and-right-cancel-each-other
  (let [bob (c/new-creature [:TUL :TUR])]
    (w/add-creature! bob [1 2])
    (w/do-cycles! (p/cycle-count (:program bob)))
    (is (= 0 (:bearing (t/creature-at [1 2]))))))

(deftest moves-north-when-bearing-is-0
  (let [bob (c/new-creature [:MOV])]
    (w/add-creature! bob [1 2])
    (w/do-cycles! (p/cycle-count (:program bob))))
  (is (= (nil? (t/creature-at [1 2]))))
  (is (= "0-1-2" (:id (t/creature-at [1 1])))))

(deftest moves-east-when-bearing-is-90
  (let [bob (c/new-creature [:TUR :MOV])]
    (w/add-creature! bob [1 2])
    (w/do-cycles! (p/cycle-count (:program bob))))
  (is (= "0-1-2" (:id (t/creature-at [2 2])))))

(deftest moves-south-when-bearing-is-180
  (let [bob (c/new-creature [:TUR :TUR :MOV])]
    (w/add-creature! bob [1 2])
    (w/do-cycles! (p/cycle-count (:program bob))))
  (is (= "0-1-2" (:id (t/creature-at [1 3])))))

(deftest moves-east-when-bearing-is-270
  (let [bob (c/new-creature [:TUL :MOV])]
    (w/add-creature! bob [1 2])
    (w/do-cycles! (p/cycle-count (:program bob))))
  (is (= "0-1-2" (:id (t/creature-at [0 2])))))

(deftest move-treats-terrain-as-a-sphere
  (let [bob (c/new-creature [:MOV :TUL :MOV])]
    (w/add-creature! bob [0 0])
    (w/do-cycles! (p/cycle-count (:program bob))))
  (let [bob (t/creature-at [(dec (get-param :world-size)) (dec (get-param :world-size))])]
    (is (= "0-0-0" (:id bob)))
    ))


; -- mating

(deftest creatures-mate-when-one-bumps-into-another
  (let [bob (c/new-creature [:MOV])]
    (w/add-creature! bob [1 1])
    (w/add-creature! (c/new-creature [:EAT]) [1 0])
    (dosync
      ; get creatures into a state in which they can definitely mate
      (t/upsert-creature-at! [1 1] (merge (t/creature-at [1 1]) {:ep 100000}))
      (t/upsert-creature-at! [1 0] (merge (t/creature-at [1 0]) {:ep 100000 :bcycle -10000})))
    (w/do-cycles! (p/cycle-count (:program bob))))
  (let [creatures (vec (t/all-creatures))]
    (is (= 3 (count creatures)))
    (is (= "0-1-1" (:id (t/creature-at [1 1]))))
    (is (= 90 (:bearing (t/creature-at [1 1]))))
    (is (= "0-1-0" (:id (t/creature-at [1 0]))))
    (is (= "5-2-1" (:id (t/creature-at [2 1]))))
    ))


; -- eating and energy points

(deftest proc-takes-one-ep-per-cycle
  (w/add-creature! (c/new-creature [:NOP]) [0 0])
  (w/do-cycles! 2)
  (let [bob (t/creature-at [0 0])]
    (is (= (- (get-param :creature-start-ep) 2) (:ep bob)))
    ))

(deftest proc-removes-creature-when-no-ep-left
  (w/add-creature! (assoc (c/new-creature [:NOP]) :ep 1) [0 0])
  (w/do-cycles! 1)
  (let [creatures (t/all-creatures)]
    (is (= 0 (count creatures)))
    ))

(deftest eat-transfers-ep-from-plant-to-creature
  (let [plant-before (c/new-plant)
        bob (c/new-creature [:EAT])]
    (w/add-plant! plant-before [0 0])
    (w/add-creature! bob [0 0])
    (w/do-cycles! (p/cycle-count [:EAT]))
    (let [bob (t/creature-at [0 0])
          plant-after (t/plant-at [0 0])
          expected-creature-ep (- (+ (get-param :creature-start-ep) (get-param :eat-ep)) (p/cycle-count [:EAT]))
          expected-plant-ep (- (:ep plant-before) (get-param :eat-ep))]
      (is (= expected-creature-ep (:ep bob)))
      (is (= expected-plant-ep (:ep plant-after)))
      )))

(deftest eat-removes-plant-when-no-ep-left
  (let [plant (assoc (c/new-plant) :ep 12)
        bob (c/new-creature [:EAT])]
    (w/add-plant! plant [0 0])
    (w/add-creature! bob [0 0])
    (w/do-cycles! (p/cycle-count [:EAT]))
    (let [bob (t/creature-at [0 0])
          expected-ep (- (+ (get-param :creature-start-ep) 12) (p/cycle-count [:EAT]))]
      (is (= expected-ep (:ep bob)))
      (is (nil? (t/plant-at [0 0])))
      )))

(deftest eat-does-not-exceed-creatures-max-ep
  (let [plant-before (c/new-plant)
        bob (assoc (c/new-creature [:EAT]) :ep (- (get-param :creature-max-ep) 10))]
    (w/add-plant! plant-before [0 0])
    (w/add-creature! bob [0 0])
    (w/do-cycles! (p/cycle-count [:EAT]))
    (let [bob (t/creature-at [0 0])
          plant-after (t/plant-at [0 0])
          expected-ep (- (:ep plant-before) 10 (p/cycle-count [:EAT]))]
      (is (= (get-param :creature-max-ep) (:ep bob)))
      (is (= expected-ep (:ep plant-after)))
      )))


; -- jumps

(deftest jump-to-pc-zero
  (w/add-creature! (c/new-creature [:NOP :JMZ :EAT :MOV :TUL]) [0 0])
  (w/do-cycles! (p/cycle-count [:NOP :JMZ]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :NOP))
    ))

(deftest jump-rel-2
  (w/add-creature! (c/new-creature [:NOP :JR2 :EAT :MOV :TUL]) [0 0])
  (w/do-cycles! (p/cycle-count [:NOP :JR2]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :MOV))
    ))

(deftest jump-rel-4
  (w/add-creature! (c/new-creature [:JR4 :NOP :EAT :MOV :TUL]) [0 0])
  (w/do-cycles! (p/cycle-count [:JR4]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :TUL))
    ))

(deftest jump-to-next-section
  (set-param! :psection-length 4)
  (w/add-creature! (c/new-creature [:NOP :JMS :MOV :NOP :EAT]) [0 0])
  (w/do-cycles! (p/cycle-count [:NOP :JMS]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :EAT))
    ))

(deftest jump-to-next-section-when-last-in-section
  (set-param! :psection-length 4)
  (w/add-creature! (c/new-creature [:NOP :NOP :NOP :JMS :EAT]) [0 0])
  (w/do-cycles! (p/cycle-count [:NOP :NOP :NOP :JMS]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :EAT))
    ))

(deftest jump-to-next-section-when-first-in-section
  (set-param! :psection-length 4)
  (w/add-creature! (c/new-creature [:JMS :NOP :NOP :NOP :EAT]) [0 0])
  (w/do-cycles! (p/cycle-count [:JMS]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :EAT))
    ))

(deftest jump-back-to-this-section
  (set-param! :psection-length 4)
  (w/add-creature! (c/new-creature [:NOP :JTS :TUL :TUL :TUR]) [0 0])
  (w/do-cycles! (p/cycle-count [:NOP :JTS]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :NOP))
    ))

(deftest jump-back-to-this-section-when-first-in-section
  (set-param! :psection-length 4)
  (w/add-creature! (c/new-creature [:JTS :NOP]) [0 0])
  (w/do-cycles! (p/cycle-count [:JTS]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :JTS))
    ))


; -- branches

(deftest branch-true-jumps-to-next-section-when-cflag-is-set
  (set-param! :psection-length 4)
  (w/add-creature! (assoc (c/new-creature [:NOP :BTR :MOV :NOP :EAT]) :cflag true) [0 0])
  (w/do-cycles! (p/cycle-count [:NOP :BTR]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :EAT))
    ))

(deftest branch-true-continues-when-cflag-is-not-set
  (set-param! :psection-length 4)
  (w/add-creature! (assoc (c/new-creature [:NOP :BTR :MOV :NOP :EAT]) :cflag false) [0 0])
  (w/do-cycles! (p/cycle-count [:NOP :BTR]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :MOV))
    ))

(deftest branch-not-true-jumps-to-next-section-when-cflag-is-not-set
  (set-param! :psection-length 4)
  (w/add-creature! (assoc (c/new-creature [:NOP :BNT :MOV :NOP :EAT]) :cflag false) [0 0])
  (w/do-cycles! (p/cycle-count [:NOP :BNT]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :EAT))
    ))

(deftest branch-not-true-continues-when-cflag-is-set
  (set-param! :psection-length 4)
  (w/add-creature! (assoc (c/new-creature [:NOP :BNT :MOV :NOP :EAT]) :cflag true) [0 0])
  (w/do-cycles! (p/cycle-count [:NOP :BNT]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :MOV))
    ))


; -- clear

(deftest clear-check-flag-resets-cflag
  (w/add-creature! (assoc (c/new-creature [:CLC]) :cflag true) [0 0])
  (w/do-cycles! (p/cycle-count [:CLC]))
  (let [bob (t/creature-at [0 0])]
    (is (false? (:cflag bob)))
    ))


; -- checks

(deftest check-food-here-sets-cflag-when-food-is-here
  (w/add-plant! (c/new-plant) [0 0])
  (w/add-creature! (c/new-creature [:CFH]) [0 0])
  (w/do-cycles! (p/cycle-count [:CFH]))
  (let [bob (t/creature-at [0 0])]
    (is (true? (:cflag bob)))
    ))

(deftest check-food-here-resets-cflag-when-no-food-is-here
  (w/add-creature! (assoc (c/new-creature [:CFH]) :cflag true) [0 0])
  (w/do-cycles! (p/cycle-count [:CFH]))
  (let [bob (t/creature-at [0 0])]
    (is (false? (:cflag bob)))
    ))

(deftest check-food-ahead-sets-cflag-when-food-is-in-viewing-distance
  (set-param! :view-distance 10)
  (w/add-plant! (c/new-plant) [10 0])
  (w/add-creature! (c/new-creature [:TUR :CFA]) [0 0])
  (w/do-cycles! (p/cycle-count [:TUR :CFA]))
  (let [bob (t/creature-at [0 0])]
    (is (true? (:cflag bob)))
    ))

(deftest check-food-ahead-resets-cflag-when-no-food-is-in-viewing-distance
  (set-param! :view-distance 10)
  (w/add-plant! (c/new-plant) [11 0])
  (w/add-creature! (assoc (c/new-creature [:TUR :CFA]) :cflag true) [0 0])
  (w/do-cycles! (p/cycle-count [:TUR :CFA]))
  (let [bob (t/creature-at [0 0])]
    (is (false? (:cflag bob)))
    ))

; -- combined check and branch

(deftest branch-food-here-jumps-to-next-section-when-food-is-here
  (set-param! :psection-length 4)
  (w/add-plant! (c/new-plant) [0 0])
  (w/add-creature! (c/new-creature [:BFH :MOV :NOP :NOP :EAT]) [0 0])
  (w/do-cycles! (p/cycle-count [:BFH]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :EAT))
    ))

(deftest branch-food-here-continues-when-no-food-is-here
  (set-param! :psection-length 4)
  (w/add-creature! (c/new-creature [:BFA :MOV :NOP :NOP :EAT]) [0 0])
  (w/do-cycles! (p/cycle-count [:BFH]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :MOV))
    ))

(deftest branch-food-ahead-jumps-to-next-section-when-food-is-in-view-distance
  (set-param! :psection-length 4)
  (set-param! :view-distance 10)
  (w/add-plant! (c/new-plant) [10 0])
  (w/add-creature! (c/new-creature [:TUR :BFA :MOV :NOP :EAT]) [0 0])
  (w/do-cycles! (p/cycle-count [:TUR :BFA]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :EAT))
    ))

(deftest branch-food-ahead-continues-when-no-food-is-in-view-distance
  (set-param! :psection-length 4)
  (set-param! :view-distance 10)
  (w/add-plant! (c/new-plant) [11 0])
  (w/add-creature! (c/new-creature [:TUR :BFA :MOV :NOP :EAT]) [0 0])
  (w/do-cycles! (p/cycle-count [:TUR :BFA]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :MOV))
    ))

(deftest branch-food-ahead-continues-when-food-is-here-but-not-ahead
  (set-param! :psection-length 4)
  (set-param! :view-distance 10)
  (w/add-plant! (c/new-plant) [0 0])
  (w/add-creature! (c/new-creature [:TUR :BFA :MOV :NOP :EAT]) [0 0])
  (w/do-cycles! (p/cycle-count [:TUR :BFA]))
  (let [bob (t/creature-at [0 0])]
    (is (= (c/current-instr bob) :MOV))
    ))
