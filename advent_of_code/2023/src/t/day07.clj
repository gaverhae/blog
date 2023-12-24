(ns t.day07
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line] (s/split line #" ")))
       (map (fn [[a b]] [a (parse-long b)]))))




(defn part1
  [input]
  (let [values (->> "AKQJT98765432"
                    (map-indexed (fn [i c] [c i]))
                    (into {}))
        score (fn [hand] (case (sort (vals (frequencies hand)))
                           [5] 0
                           [1 4] 1
                           [2 3] 2
                           [1 1 3] 3
                           [1 2 2] 4
                           [1 1 1 2] 5
                           [1 1 1 1 1] 6))]
    (->> input
         (map (fn [[a b]] [(score a)
                           (->> a (map values) vec)
                           b]))
         sort
         reverse
         (map-indexed (fn [idx [_ _ bid]]
                        (* (inc idx) bid)))
         (reduce + 0))))


(defn part2
  [input]
  (let [values (->> "AKQT98765432J"
                    (map-indexed (fn [i c] [c i]))
                    (into {}))
        score (fn [hand]
                (let [others (->> hand (remove #{\J}) frequencies)]

                (case (sort (vals others))
                  [5] 0
                  [1 4] 1
                  [2 3] 2
                  [1 1 3] 3
                  [1 2 2] 4
                  [1 1 1 2] 5
                  [1 1 1 1 1] 6
                  [4] 0
                  [1 3] 1
                  [2 2] 2
                  [1 1 2] 3
                  [1 1 1 1] 5
                  [3] 0
                  [1 2] 1
                  [1 1 1] 3
                  [2] 0
                  [1 1] 1
                  [1] 0
                  [] 0)))]
    (->> input
         (map (fn [[a b]] [(score a)
                           (->> a (map values) vec)
                           b]))
         sort
         reverse
         (map-indexed (fn [idx [_ _ bid]]
                        (* (inc idx) bid)))
         (reduce + 0))))

(lib/check
  [part1 sample] 6440
  [part1 puzzle] 248569531
  [part2 sample] 5905
  [part2 puzzle] 250382098)
