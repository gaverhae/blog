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
  {:hoisted {2 0, 4 4, 7 3, 8 2, 10 4, 11 -1}
   :code [[:load 0 100]
          [:load 1 1000]
          [:not= 3 2 1]
          [:jump-if-zero 3 11]
          [:add 5 0 4]
          [:add 6 5 0]
          [:add 0 6 7]
          [:add 9 0 8]
          [:add 0 9 10]
          [:add 1 11 1]
          [:jump 2]
          [:return 0]]})

(deftest registers
  (is (= register-code (t/compile-register-ssa t/ast)))
  (is (= -13 (t/run-registers register-code)))
  (is (= -13 ((t/registers-jump register-code)))))
