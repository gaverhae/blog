(ns t.core-test
  (:require [clojure.test :refer :all]
            [t.core :as t]))

(deftest arith-syntax
  (are [x y] (= x (t/parse-arith y))
       [:false] "false"
       [:true] "true"
       [:if [:true]
        [:zero]
        [:succ [:succ [:zero]]]] "if (true) then (0) else (succ (succ (0)))"))

(deftest eval-arith
  (are [x y] (= x (t/eval-arith (t/parse-arith y)))
       true "true"
       false "false"
       0 "0"
       1 "succ (0)"
       1 "if (false) then 0 else (succ 0)"
       true "iszero (pred (succ (0)))"))
