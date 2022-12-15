(ns t.day06
  (:require [t.lib :as lib]))

(def parse identity)

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
  [part1 sample] 7
  [part1 puzzle] 1896
  [part2 sample] 19
  [part2 puzzle] 3452)
