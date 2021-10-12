(ns t.core-test
  (:require [clojure.test :refer :all]
            [t.core :as t]))

(deftest lang-3-1
  (are [v] (t/value-3-1 v)
       true
       false)
  (are [t] (t/term-3-1 t)
       true
       false
       '(if true false true)
       '(if (if true false true) true (if false false true)))
  (are [not-t] (not (t/term-3-1 not-t))
       1
       nil
       ()
       [:a :b :c]
       '(if true 4 false))
  (are [out in] (= out (t/step-3-1 in))
       [:not-a-term 4] 4
       [:value true] true
       [:value false] false
       [:if-true true] '(if true true false)
       [:if-false false] '(if false true false)
       [:if-true '(if true true false)] '(if true (if true true false) false)
       [:if '(if true true false)] '(if (if false false true) true false)))
