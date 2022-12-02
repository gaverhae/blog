(ns t.lib
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest are is testing]]))

(defn parse-integers
  [lines]
  (vec (map #(Long/parseLong %) lines)))

(defn transpose
  [s]
  (apply mapv vector s))

(defmacro check
  [parse & [p1 s1 f1 p2 s2 f2]]
  `(deftest ~'check
     ~@(->> [[p1 s1 "sample"] [p1 f1 "full"] [p2 s2 "sample"] [p2 f2 "full"]]
            (keep (fn [[f e input]]
                    (when (and f e)
                      `(is (= (~f (-> (str (string/replace (ns-name ~*ns*) "t." "data/")
                                           "-"
                                           ~input)
                                      slurp
                                      string/split-lines
                                      ~parse))
                              ~e))))))))

(deftest tests
  (testing "parse-integers"
    (are [expected inputs] (= expected (parse-integers inputs))
         [1 2 3] ["1" "2" "3"]))
  (testing "transpose"
    (are [expected actual] (= expected actual)
         [[1 4] [2 5] [3 6]] (transpose [[1 2 3] [4 5 6]]))))
