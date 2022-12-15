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
  #_(let [;target-row 10
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
    (count (set/difference ranges beacons)))
  0)

(defn part2
  [input]
  (let [;limit 20
        limit 4000000
        beacon? (->> input (map second) set)
        distances (->> input (map (fn [[sensor nearest-beacon]]
                                    [sensor (manhattan sensor nearest-beacon)])))
        [y x] (->> input
                   (mapcat (fn [[[y x] b]]
                             (let [d (inc (manhattan [y x] b))]
                               (for [dy (range (- d) (inc d))
                                     xdir [1 -1]
                                     :let [dx (* xdir (Math/abs ^long (- d dy)))
                                           p [(+ y dy) (+ x dx)]]
                                     :when (and (not (beacon? p))
                                                (<= 0 (first p) limit)
                                                (<= 0 (second p) limit)
                                                (->> distances
                                                     (every?
                                                       (fn [[sensor distance]]
                                                         (> (manhattan sensor p) distance)))))]
                                 p))))
                   first)]
    (+ (* (* 4 1000 1000) x)
       y)))


(lib/check
  parse
  part1 26 4876693
  part2 56000011 11645454855041
  )
