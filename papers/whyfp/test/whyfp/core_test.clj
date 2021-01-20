(ns whyfp.core-test
  (:require [clojure.test :refer [deftest is]]
            [whyfp.core :as core]))

(deftest section3
  (is (= 10 (core/sum [1 2 3 4])))
  (is (= 24 (core/product [1 2 3 4])))
  (is (= true (core/anytrue [false false true false])))
  (is (= false (core/alltrue [false false true false])))
  (is (= [1 2 3 4] (core/append [1 2] [3 4])))
  (is (= [2 4 6 8] (core/doubleall [1 2 3 4])))
  (is (= 21 (core/summatrix [[1 2 3] [4 5 6]])))
  (is (= 10 (core/sumtree' core/tree')))
  (is (= 10 (core/sumtree core/tree)))
  (is (= [1 2 3 4] (core/labels core/tree)))
  (is (= [1 2 3 4] (core/labels' core/tree')))
  (is (= [:node 2 [:cons [:node 4 nil]
                         [:cons [:node 6 [:cons [:node 8 nil] nil]]
                                nil]]]
         ((core/maptree #(* 2 %)) core/tree)))
  (is (= [2 [4] [6 [8]]] ((core/maptree' #(* 2 %)) core/tree'))))
