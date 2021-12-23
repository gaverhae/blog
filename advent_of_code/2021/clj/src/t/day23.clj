(ns t.day23
  (:require [t.util :refer [transpose]]))

(defn parse
  [lines]
  {:hallway [0 0 0 0 0 0 0 0 0 0 0]
   :halls (->> lines
               (drop 2)
               (take 2)
               (map (fn [line] (keep {\A 1, \B 10, \C 100, \D 1000} line)))
               transpose
               (mapv vec))})

(defn part1
  [input])

(defn part2
  [input])
