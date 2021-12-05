(ns t.day5
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [t.util :refer [transpose]]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)]
                [[(Long/parseLong x1) (Long/parseLong y1)]
                 [(Long/parseLong x2) (Long/parseLong y2)]])))))

(defn part1
  [input]
  (->> input
       (filter (fn [[[x1 y1] [x2 y2]]]
                 (or (= x1 x2) (= y1 y2))))
       (mapcat (fn [[[x1 y1] [x2 y2]]]
                 (for [x (range (min x1 x2) (inc (max x1 x2)))
                       y (range (min y1 y2) (inc (max y1 y2)))]
                   [x y])))
       frequencies
       vals
       (filter (fn [x] (>= x 2)))
       count))


(defn part2
  [input]
  (->> input
       (mapcat (fn [[[x1 y1] [x2 y2]]]
                 (let [[dx dy] [(cond (= x1 x2) 0
                                      (< x1 x2) 1
                                      :else -1)
                                (cond (= y1 y2) 0
                                      (< y1 y2) 1
                                      :else -1)]
                       num-step (max (Math/abs (long (- x1 x2))) (Math/abs (long (- y1 y2))))]
                   (for [n (range (inc num-step))]
                     [(+ x1 (* n dx))
                      (+ y1 (* n dy))]))))
       frequencies
       vals
       (filter (fn [x] (>= x 2)))
       count))
