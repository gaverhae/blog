(ns t.day5
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [t.util :refer [transpose]]))

(defn sign
  [x]
  (cond (zero? x) 0
        (pos? x) 1
        (neg? x) -1))

(defn count-overlaps
  [segments]
  (->> segments
       (mapcat (fn [[x1 y1 x2 y2]]
                 (let [[dx dy] [(sign (- x2 x1)) (sign (- y2 y1))]
                       num-step (max (Math/abs (long (- x1 x2)))
                                     (Math/abs (long (- y1 y2))))]
                   (for [n (range (inc num-step))]
                     [(+ x1 (* n dx))
                      (+ y1 (* n dy))]))))
       frequencies
       vals
       (filter (fn [x] (>= x 2)))
       count))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (->> (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)
                   rest
                   (map #(Long/parseLong %)))))))

(defn part1
  [input]
  (->> input
       (filter (fn [[x1 y1 x2 y2]]
                 (or (= x1 x2) (= y1 y2))))
       count-overlaps))

(defn part2
  [input]
  (->> input
       count-overlaps))
