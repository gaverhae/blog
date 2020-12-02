(ns t.core-test
  (:require [clojure.test :refer :all]
            [t.core :as t]))

(deftest day-1-part-1
  (is (= [1721 299]
         (t/sum-2020 [1721
                      979
                      366
                      299
                      675
                      1456])))
  (is (= 712075 (t/day-1-part-1))))

