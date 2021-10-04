(ns t.arith.syntax-test
  (:require [clojure.test :refer [deftest are]]
            [t.arith.syntax :as t]))

(deftest arith-syntax
  (are [x y] (= x (t/parse y))
       [:false] "false"
       [:true] "true"
       [:if [:true]
        [:zero]
        [:succ [:succ [:zero]]]] "if (true) then (0) else (succ (succ (0)))"))

(deftest eval-arith
  (are [x y] (= x (t/eval (t/parse y)))
       true "true"
       false "false"
       0 "0"
       1 "succ (0)"
       1 "if (false) then 0 else (succ 0)"
       true "iszero (pred (succ (0)))"
       0 "pred (pred (pred 0))"
       0 "pred (succ 0)"
       1 "succ 0"
       1 "succ (pred 0)"
       2 "pred (succ (succ (succ 0)))"))
