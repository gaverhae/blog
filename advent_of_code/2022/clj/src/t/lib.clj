(ns t.lib
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest are is testing]]
            [hato.client :as hc]))

(defn ->long
  [s]
  (Long/parseLong s))

(defn transpose
  [s]
  (apply mapv vector s))

(defmacro check
  [& specs]
  `(deftest ~'check
     (let [~'day (-> (ns-name ~*ns*)
                     (string/replace "t.day" "")
                     (Long/parseLong))
           ~'sample (delay (-> (format "data/day%02d-sample" ~'day)
                               slurp
                               string/split-lines
                               ~'parse))
           ~'puzzle (delay
                      (let [file# (format "data/day%02d-puzzle" ~'day)]
                        (when (not (.exists (io/file file#)))
                          (spit file#
                                (-> (format "https://adventofcode.com/2022/day/%d/input"
                                            ~'day)
                                    (hc/get {:headers
                                             {"cookie" (format "session=%s"
                                                               (System/getenv "AOC_SESSION"))}})
                                    :body)))
                        (-> file#
                            slurp
                            string/split-lines
                            ~'parse)))]
       ~@(->> specs
              (partition 2)
              (map (fn [[[part input & args] expected]]
                     `(is (= ~(concat [part `(force ~input)] args) ~expected))))))))

(deftest tests
  (testing "transpose"
    (are [expected actual] (= expected actual)
         [[1 4] [2 5] [3 6]] (transpose [[1 2 3] [4 5 6]]))))
