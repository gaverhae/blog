(ns t.day06
  (:require [t.lib :as lib]))

(defn parse
  [lines]
  (first lines))

(defn solve
  [line size]
  (->> line
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
  parse
  part1 7 1896
  part2 19 3452)
