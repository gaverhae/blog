(ns t.day06
  (:require [t.lib :as lib]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn parse
  [lines]
  (first lines))

(defn solve
  [line size]
  (->> line
       (partition size 1)
       (map-indexed (fn [idx e]
                      (when (= size (count (set e)))
                        idx)))
       (filter identity)
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
