(ns t.core-test
  (:require [clojure.test :refer :all]
            [t.core :as t]))

(deftest the-test
  (is (= [1 2 3 4 5]
         (take 5 t/integers)))
  (is (= [1 2 4 8 16]
         (t/divisors 16)))
  (are [x y] (= y (t/prime? x))
       1 false
       2 true
       3 true
       4 false
       5 true
       6 false
       7 true
       8 false
       9 false
       10 false
       11 true
       16 false
       17 true
       51 false
       53 true)
  (is (= [2 3 5 7 11 13 17 19 23 29]
         (take 10 t/primes)))
  (are [x y] (= y (t/nth-prime x))
       0 2
       1 3
       2 5
       100 547
       1000 7927))
