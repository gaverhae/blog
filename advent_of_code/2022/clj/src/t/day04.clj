(ns t.day04
  (:require [t.lib :as lib :refer [->long]]
            [clojure.set :as set]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [[_ a b c d] (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" line)
                    sections (fn [a b]
                               (set (range (->long a) (inc (->long b)))))]
                [(sections a b) (sections c d)])))))

(defn part1
  [input]
  (->> input
       (filter (fn [[p1 p2]]
                 (or (set/subset? p1 p2)
                     (set/subset? p2 p1))))
       count))

(defn part2
  [input]
  (->> input
       (filter (fn [[p1 p2]]
                 (seq (set/intersection p1 p2))))
       count))

(lib/check
  parse
  part1 2 424
  part2 4 804)
