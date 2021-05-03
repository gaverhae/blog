(ns t.core-test
  (:require [clojure.test :refer [deftest is]]
            [t.core :as t]))

(deftest ast-walks
  (is (= -13 (t/baseline)))
  (is (= -13 (t/naive-ast-walk t/ast)))
  (is (= -13 ((t/compile-to-closure t/ast)))))

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
   [:get 0]
   [:end]])

(deftest stacks
  (is (= stack-code (t/compile-stack t/ast)))
  (is (= -13 (t/run-stack stack-code)))
  (is (= -13 ((t/stack-exec-cont stack-code))))
  (is (= -13 ((t/stack-exec-mut stack-code))))
  (is (= -13 ((t/stack-exec-case stack-code))))
  (is (= -13 ((t/stack-exec-case-jump stack-code)))))

(def register-code
  {:hoisted {4 0, 7 4, 10 3, 12 2, 14 4, 16 -1}
   :code [[:load 0 100]
          [:load 1 1000]
          [:not= 5 4 1]
          [:jump-if-zero 5 11]
          [:add 8 0 7]
          [:add 9 8 0]
          [:add 0 9 10]
          [:add 13 0 12]
          [:add 0 13 14]
          [:add 1 16 1]
          [:jump 2]
          [:return 0]]})

(deftest registers
  (is (= register-code (t/compile-register-ssa t/ast)))
  (is (= -13 (t/run-registers register-code))))
