(ns t.day1
  (:require [t.util :as util]))

(def parse util/parse-integers)

(defn part1
  [input]
  (->> input
       (partition 2 1)
       (filter (fn [[a b]] (< a b)))
       count))

(defn part2
  [input]
  (->> input
       (partition 3 1)
       (map (fn [v] (reduce + 0 v)))
       part1))
