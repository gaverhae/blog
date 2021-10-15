(ns t.core-test
  (:require [clojure.test :refer :all]
            [t.core :as t]))

(deftest lang-3-1
  (are [v] (t/b-3-1-value? v)
       true
       false)
  (are [t] (t/b-3-1-term? t)
       true
       false
       '(if true false true)
       '(if (if true false true) true (if false false true)))
  (are [v] (t/b-3-1-normal? v)
       true
       false)
  (are [not-t] (not (t/b-3-1-term? not-t))
       1
       nil
       ()
       [:a :b :c]
       '(if true 4 false))
  (are [out in] (= out (t/b-3-1-step in))
       true '(if true true false)
       false '(if false true false)
       '(if true true false) '(if true (if true true false) false)
       '(if true true false) '(if (if false false true) true false))
  (are [out in] (= out (t/b-3-1-eval in))
       [:value true] '(if true true false)
       [:value false] '(if false true false)
       [:value true] '(if true (if true true false) false)
       [:value true] '(if (if false false true) true false)))

(deftest lang-3-2
  (are [v] (t/b-3-2-value? v)
       true
       false
       0
       '(succ (succ (succ 0))))
  (are [t] (t/b-3-2-term? t)
       true
       false
       '(if true (zero? (succ 0)) true)
       '(if (if true false true) true (if false false true))
       '(succ (if true false (pred 0))))
  (are [v] (t/b-3-2-normal? v)
       true
       false
       0
       '(succ (succ 0))
       '(succ (succ (pred true)))
       '(if 0 true false)
       '(pred false))
  (are [not-t] (not (t/b-3-2-term? not-t))
       1
       nil
       ()
       [:a :b :c]
       '(if true 4 false))
  (are [out in] (= out (t/b-3-2-step in))
       true '(if true true false)
       false '(if false true false)
       '(if true true false) '(if true (if true true false) false)
       '(if true true false) '(if (if false false true) true false)
       0 '(pred 0)
       0 '(pred (succ 0)))
  (are [out in] (= out (t/b-3-2-eval in))
       [:value true] '(if true true false)
       [:value false] '(if false true false)
       [:value true] '(if true (if true true false) false)
       [:value true] '(if (if false false true) true false)
       [:value 0] '(pred (succ (pred 0)))
       [:value true] '(if (zero? (pred (succ (pred 0)))) true false)
       [:error '(if 0 true false)] '(if 0 true false)
       [:error '(pred (succ (pred (succ (succ true)))))]
       '(if (zero? (pred (succ (pred 0))))
          (pred (succ (pred (succ (succ true)))))
          0)))
