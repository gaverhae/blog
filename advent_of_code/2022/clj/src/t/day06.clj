(ns t.day06
  (:require [t.lib :as lib]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn parse
  [lines]
  (first lines))

(defn part1
  [input]
  (->> input
       (partition 4 1)
       (map-indexed (fn [idx e]
                      (when (= 4 (count (set e)))
                        idx)))
       (filter identity)
       first
       (+ 4)))

(defn part2
  [input]
  (->> input
       (partition 14 1)
       (map-indexed (fn [idx e]
                      (when (= 14 (count (set e)))
                        idx)))
       (filter identity)
       first
       (+ 14)))

(lib/check
  parse
  part1 7 1896
  part2 19 3452)
