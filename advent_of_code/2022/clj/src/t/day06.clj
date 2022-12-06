(ns t.day06
  (:require [t.lib :as lib]))

(defn solve
  [input size]
  (->> input
       first
       (partition size 1)
       (keep-indexed (fn [idx e]
                      (when (= size (count (set e)))
                        idx)))
       first
       (+ size)))

(defn part1
  [input]
  (solve input 4))

(defn part2
  [input]
  (solve input 14))

(lib/check
  identity
  part1 7 1896
  part2 19 3452)
