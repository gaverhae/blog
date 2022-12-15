(ns t.day01
  (:require [t.lib :as lib :refer [->long]]))

(defn parse
  [input]
  (->> input
       (partition-by #(= % ""))
       (remove #{[""]})
       (map (fn [s] (reduce + 0 (map ->long s))))
       sort
       reverse))

(defn part1
  [input]
  (first input))

(defn part2
  [input]
  (reduce + 0 (take 3 input)))

(lib/check
  [part1 sample] 24000
  [part1 puzzle] 75622
  [part2 sample] 45000
  [part2 puzzle] 213159)
