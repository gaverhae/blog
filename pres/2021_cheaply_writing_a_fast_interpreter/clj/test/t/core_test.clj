(ns t.core-test
  (:require [clojure.test :refer [deftest is]]
            [t.core :as t]))

(deftest ast-walks
  (is (= -13 (t/baseline)))
  (is (= [-13 [-13 0]] (t/naive-ast-walk t/ast [])))
  (is (= [-13 [-13 0]] ((t/compile-to-closure t/ast) []))))

(def stack-code
  [[:push 100]
   [:set 0]
   [:push 1000]
   [:set 1]
   [:push 0]
   [:get 1]
   [:not=]
   [:jump-if-zero 27]
   [:get 0]
   [:push 4]
   [:add]
   [:get 0]
   [:add]
   [:push 3]
   [:add]
   [:set 0]
   [:get 0]
   [:push 2]
   [:add]
   [:push 4]
   [:add]
   [:set 0]
   [:push -1]
   [:get 1]
   [:add]
   [:set 1]
   [:jump 4]
   [:get 0]])

(deftest stacks
  (is (= stack-code (t/compile-stack t/ast)))
  (is (= [-13 [-13 0 -13]] (t/run-stack stack-code)))
  (is (= [-13 0 -13] ((t/stack-exec-cont stack-code))))
  (is (= [-13 0 -13] ((t/stack-exec-mut stack-code))))
  (is (= [-13 0 -13] ((t/stack-exec-case stack-code))))
  (is (= [-13 0 -13] ((t/stack-exec-case-jump stack-code)))))

(def register-code
  [[:load 2 100]
   [:loadr 0 2]
   [:load 4 1000]
   [:loadr 1 4]
   [:load 6 0]
   [:loadr 7 1]
   [:not= 8 6 7]
   [:jump-if-zero 8 27]
   [:loadr 10 0]
   [:load 11 4]
   [:add 12 10 11]
   [:loadr 13 0]
   [:add 14 12 13]
   [:load 15 3]
   [:add 16 14 15]
   [:loadr 0 16]
   [:loadr 18 0]
   [:load 19 2]
   [:add 20 18 19]
   [:load 21 4]
   [:add 22 20 21]
   [:loadr 0 22]
   [:load 24 -1]
   [:loadr 25 1]
   [:add 26 24 25]
   [:loadr 1 26]
   [:jump 4]
   [:loadr 29 0]])

(deftest registers
  (is (= register-code (t/compile-register-ssa t/ast)))
  (is (= [[0 -13] [1 0] [2 100] [4 1000] [6 0] [7 0] [8 0] [10 -13] [11 4] [12 -9] [13 -13] [14 -22] [15 3] [16 -19] [18 -19] [19 2] [20 -17] [21 4] [22 -13] [24 -1] [25 1] [26 0] [29 -13]]
         (t/run-registers register-code)))
  (is (= [[0 -13] [1 0] [2 100] [4 1000] [6 0] [7 0] [8 0] [10 -13] [11 4] [12 -9] [13 -13] [14 -22] [15 3] [18 -19] [19 2] [20 -17] [21 4] [24 -1] [25 1] [29 -13]]
         (t/run-registers (t/optimize-register-code register-code)))))
