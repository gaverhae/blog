(ns t.day01
  (:require [t.lib :as lib]))

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

(lib/check
  parse
  part1 24000 75622
  part2 45000 213159)
