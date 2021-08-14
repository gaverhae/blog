(ns t.core-test
  (:require [clojure.test :refer [deftest is]]
            [t.core :as t]))

(deftest ast-walks
  (is (= -13 (t/baseline)))
  (is (= -13 (t/naive-ast-walk t/ast)))
  (is (= -13 (t/twe-mon t/ast)))
  (is (= -13 (trampoline (t/twe-cont t/ast))))
  (is (= -13 ((t/compile-to-closure t/ast)))))

(def stack-code
  [[:push 100]
   [:set 0]
   [:push 1000]
   [:set 1]
   [:push 0]
   [:get 1]
   [:bin :not=]
   [:jump-if-zero 27]
   [:get 0]
   [:push 4]
   [:bin :add]
   [:get 0]
   [:bin :add]
   [:push 3]
   [:bin :add]
   [:set 0]
   [:get 0]
   [:push 2]
   [:bin :add]
   [:push 4]
   [:bin :add]
   [:set 0]
   [:push -1]
   [:get 1]
   [:bin :add]
   [:set 1]
   [:jump 4]
   [:get 0]
   [:end]])

(deftest stacks
  (is (= stack-code (t/compile-stack t/ast)))
  (is (= -13 (t/run-stack stack-code)))
  (is (= -13 ((t/run-stack-mut stack-code)))))

(def register-code
  {:hoisted {2 0, 4 4, 7 3, 8 2, 10 4, 11 -1}
   :code [[:loadl 0 100]
          [:loadl 1 1000]
          [[:bin :not=] 3 2 1]
          [:jump-if-zero 3 11]
          [[:bin :add] 5 0 4]
          [[:bin :add] 6 5 0]
          [[:bin :add] 0 6 7]
          [[:bin :add] 9 0 8]
          [[:bin :add] 0 9 10]
          [[:bin :add] 1 11 1]
          [:jump 2]
          [:return 0]]
   :reg 12})

(deftest registers
  (is (= register-code (t/compile-register t/ast)))
  (is (= -13 ((t/run-registers register-code))))
  (is (= -13 ((t/registers-jump register-code))))
  (is (= -13 ((t/registers-loop register-code))))
  (is (= -13 (first (t/registers-c register-code 1)))))
