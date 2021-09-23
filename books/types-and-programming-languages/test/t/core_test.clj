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

(deftest consts
  (are [x y] (= x (t/consts (t/parse-arith y)))
       #{true} "true"
       #{false} "false"
       #{true 0} "if true then (succ 0) else (pred 0)"))

(deftest size
  (are [x y] (= x (t/size (t/parse-arith y)))
       1 "true"
       1 "false"
       5 "if true then (succ 0) else (pred 0)"))

(deftest depth
  (are [x y] (= x (t/depth (t/parse-arith y)))
       1 "true"
       1 "false"
       3 "if true then (succ 0) else (pred 0)"))
