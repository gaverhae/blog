(ns t.day01
  (:require [t.lib :as lib]
            [clojure.string :as string]
            [clojure.test :refer [deftest are]]))

(defn parse
  [input]
  (->> input
       (partition-by #(= % ""))
       (remove #{[""]})
       (map (fn [s] (reduce + 0 (map (fn [i] (Integer/parseInt i)) s))))
       sort
       reverse))

(defn part1
  [input]
  (first input))

(defn part2
  [input]
  (reduce + 0 (take 3 input)))

(deftest tests
  (are [expected input f]
       (= expected (-> (slurp (str "data/day01-" input))
                       string/split-lines
                       parse
                       f))
       24000 "sample" part1
       75622 "full" part1
       45000 "sample" part2
       213159 "full" part2))
