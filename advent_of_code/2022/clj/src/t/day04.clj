(ns t.day04
  (:require [t.lib :as lib]
            [clojure.set :as set]
            [clojure.string :as string]))

(defn parse
  [lines]
  (->> lines
       (map (fn [l]
              (map (fn [p] (map (fn [i] (Long/parseLong i))
                                (string/split p #"-")))
                   (string/split l #","))))
       (map (fn [l]
              (map (fn [[a b]]
                     (set (range a (inc b))))
                   l)))))

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
                 (not (empty? (set/intersection p1 p2)))))
       count))

(lib/check
  parse
  part1 2 424
  part2 4 0
  )
