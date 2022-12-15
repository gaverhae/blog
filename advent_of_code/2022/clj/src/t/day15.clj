(ns t.day15
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [re #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
                    [_ sx sy bx by] (re-matches re line)]
                [[(->long sy) (->long sx)] [(->long by) (->long bx)]])))))

(defn manhattan
  [[^long x1 ^long y1] [^long x2 ^long y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn part1
  [input]
  #_(let [ds (->> input
                (map (fn [[s b]] [s (manhattan s b)])))
        grid (reduce (fn [[xmin xmax ymin ymax] [[y1 x1] [y2 x2]]]
                       [(min xmin x1 x2)
                        (max xmax x1 x2)
                        (min ymin y1 y2)
                        (max ymax y1 y2)])
                     [0 0 0 0]
                     input)
        beacon? (->> input
                     (map second)
                     set)]
    (prn grid)
    (count (for [y #_[2000000] [10]
                 x (range (get grid 0) (inc (get grid 1)))
                 :let [p [y x]]
                 :when (and (not (beacon? p))
                            (some (fn [[s d]]
                                    (<= (manhattan p s) d))
                                  ds))]
             p)))
  (let [;target-row 10
        target-row 2000000
        beacons (->> input
                     (map second)
                     (keep (fn [[y x]]
                            (when (= y target-row)
                              x)))
                     set)
        ranges (->> input
                    (mapcat (fn [[sensor nearest-beacon]]
                              (let [no-beacon-distance (manhattan sensor nearest-beacon)
                                    left-at-target-row (- no-beacon-distance
                                                          (Math/abs ^long (- target-row (first sensor))))]
                                (when (not (neg? left-at-target-row))
                                  (range (- (second sensor) left-at-target-row)
                                         (+ 1 (second sensor) left-at-target-row))))))
                    set)]
    (count (set/difference ranges beacons))))

(defn part2
  [input]
  input
  )


(lib/check
  parse
  part1 26 4876693
  #_part2
  )
