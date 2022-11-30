(ns t.lib
  (:require [clojure.test :refer [deftest are testing]]))

(defn parse-integers
  [lines]
  (vec (map #(Long/parseLong %) lines)))

(defn transpose
  [s]
  (apply mapv vector s))

(deftest tests
  (testing "parse-integers"
    (are [expected inputs] (= expected (parse-integers inputs))
         [1 2 3] ["1" "2" "3"]))
  (testing "transpose"
    (are [expected actual] (= expected actual)
         [[1 4] [2 5] [3 6]] (transpose [[1 2 3] [4 5 6]]))))
