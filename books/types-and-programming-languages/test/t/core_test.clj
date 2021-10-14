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
       [:value true]'(if true true false)
       [:value false] '(if false true false)
       [:value true]'(if true (if true true false) false)
       [:value true]'(if (if false false true) true false)))
