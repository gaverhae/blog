(ns t.day6
  (:require [clojure.set :as set]))

(defn parse
  [lines]
  (->> lines
       (partition-by #{""})
       (remove #{[""]})
       (map #(map set %))))

(defn part1
  [input]
  (->> input
       (map #(reduce set/union %))
       (map count)
       (reduce +)))

(defn part2
  [input]
  (->> input
       (map #(reduce set/intersection %))
       (map count)
       (reduce +)))
