(ns crellinor.terrain-test
  (:require [clojure.test :refer :all]
            [crellinor.parameters :refer [get-param]]
            [crellinor.terrain :as t]
            [crellinor.creatures :as c]))


(deftest pos-ahead
  (testing "position ahead with bearing 0 (north)"
    (is (= [2 2] (t/pos-ahead [2 3] 0))))
  (testing "position ahead with bearing 90 (east)"
    (is (= [3 3] (t/pos-ahead [2 3] 90))))
  (testing "position ahead with bearing 180 (south)"
    (is (= [2 4] (t/pos-ahead [2 3] 180))))
  (testing "position ahead with bearing 270 (west)"
    (is (= [2 2] (t/pos-ahead [2 3] 0))))
  (testing "3 positions ahead with bearing 180 (south)"
    (is (= [[2 4] [2 5] [2 6]] (t/pos-ahead [2 3] 180 3))))
  )


(deftest free-pos-tries-north-first
  (t/reset-terrain!)
  (dosync
    (t/upsert-creature-at! [1 1] (c/new-creature [])))
  (is (= (t/pos-ahead [1 1] 0) (t/free-pos-near [1 1]))))

(deftest free-pos-tries-east-when-north-pos-is-not-free
  (t/reset-terrain!)
  (dosync
    (t/upsert-creature-at! [1 1] (c/new-creature []))
    (t/upsert-creature-at! (t/pos-ahead [1 1] 0) (c/new-creature [])))
  (is (= (t/pos-ahead [1 1] 90) (t/free-pos-near [1 1]))))

(deftest free-pos-returns-nil-when-no-pos-is-free
  (t/reset-terrain!)
  (dosync
    (t/upsert-creature-at! [1 1] (c/new-creature []))
    (t/upsert-creature-at! (t/pos-ahead [1 1] 0) (c/new-creature []))
    (t/upsert-creature-at! (t/pos-ahead [1 1] 90) (c/new-creature []))
    (t/upsert-creature-at! (t/pos-ahead [1 1] 180) (c/new-creature []))
    (t/upsert-creature-at! (t/pos-ahead [1 1] 270) (c/new-creature [])))
  (is (nil? (t/free-pos-near [1 1]))))


(deftest all-plants
  (dosync
    (t/upsert-plant-at! [1 1] (c/new-plant))
    (t/upsert-plant-at! [2 3] (c/new-plant)))
  (let [plants (t/all-plants)]
    (is (= 2 (count plants)))
    (is (= (get-param :plant-start-ep) (:ep (first plants))))
    (is (= [1 1] (:pos (first plants)))))
  )
