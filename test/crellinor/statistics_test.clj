(ns crellinor.statistics-test
  (:require [clojure.test :refer :all]
            [crellinor.parameters :refer [get-param]]
            [crellinor.statistics :as stats]
            [crellinor.terrain :as t]
            [crellinor.creatures :as c]))


(deftest energy-levels

  (testing "returns energy for plants and creatures"
    (t/reset-terrain!)
    (stats/reset-statistics!)
    (dosync
      (t/upsert-plant-at! [1 1] (c/new-plant))
      (t/upsert-plant-at! [2 3] (assoc (c/new-plant) :ep 100))
      (t/upsert-creature-at! [1 1] (c/new-creature []))
      (t/upsert-creature-at! [7 3] (assoc (c/new-creature []) :ep 200)))
    (let [[plant-ep creature-ep] (stats/current-energy-levels)]
      (is (= (+ (get-param :plant-start-ep) 100) plant-ep))
      (is (= (+ (get-param :creature-start-ep) 200) creature-ep))))

  (testing "creats series in persistent reference"
    (t/reset-terrain!)
    (stats/reset-statistics!)
    (dosync
      (t/upsert-plant-at! [1 1] (c/new-plant)))
    (stats/push-energy-levels!)
    (dosync
      (t/upsert-plant-at! [1 0] (c/new-plant)))
    (stats/push-energy-levels!)
    (let [series (stats/energy-level-series)]
      (is (= (get-param :plant-start-ep) (get-in series [0 0])))
      (is (= (* (get-param :plant-start-ep) 2) (get-in series [1 0])))))

)
