(ns crellinor.gp-test
  (:require [clojure.test :refer :all]
            [crellinor.parameters :refer [get-param set-param! reset-param!]]
            [crellinor.utils :refer :all]
            [crellinor.terrain :as t]
            [crellinor.creatures :as c]
            [crellinor.gp :as gp]))

(deftest uniform-crossover
  (set-param! :xsection-length 2)
  (set-fake-rand-vals! [0 1 0])
  (with-redefs [srand-int fake-rand-int]
    (let [p0 [:MOV :MOV :MOV :MOV :MOV :MOV]
          p1 [:EAT :EAT :EAT :EAT :EAT :EAT]
          c (gp/uniform-crossover p0 p1)]
      (is (= [:MOV :MOV :EAT :EAT :MOV :MOV] c))))
  (reset-param! :xsection-length)
  )


(deftest cut-n-splice
  (set-fake-rand-vals! [2 1 3])
  (with-redefs [srand-int fake-rand-int]
    (let [p0 [:TUL :TUL :TUL :EAT :TUR :TUR]
          p1 [:MOV :NOP :NOP :MOV :MOV :MOV]
          c (gp/cut-n-splice p0 p1)]
      (is (= [:TUL :TUL :TUL :NOP :NOP :TUR] c))))
  )
