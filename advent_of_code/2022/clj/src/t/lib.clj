(ns t.lib
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest are is testing]]
            [hato.client :as hc]))

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
                      (let [n (gensym)
                            file (gensym)]
                        `(let [~n (-> (ns-name ~*ns*)
                                      (string/replace "t.day" "")
                                      (Long/parseLong))
                               ~file (format "data/day%02d-%s" ~n ~input)]
                           ~(when (= input "full")
                              `(when (not (.exists (io/file ~file)))
                                 (spit ~file
                                       (-> (format "https://adventofcode.com/2022/day/%d/input" ~n)
                                           (hc/get {:headers
                                                    {"cookie" (format "session=%s"
                                                                      (System/getenv "AOC_SESSION"))}})
                                           :body))))
                           (is (= (~f (-> ~file
                                          slurp
                                          string/split-lines
                                          ~parse))
                                  ~e))))))))))

(deftest tests
  (testing "parse-integers"
    (are [expected inputs] (= expected (parse-integers inputs))
         [1 2 3] ["1" "2" "3"]))
  (testing "transpose"
    (are [expected actual] (= expected actual)
         [[1 4] [2 5] [3 6]] (transpose [[1 2 3] [4 5 6]]))))
