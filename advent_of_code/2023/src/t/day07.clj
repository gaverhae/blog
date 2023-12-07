(ns t.day07
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(def values
  (->> "AKQJT98765432"
       (map-indexed (fn [i c] [c i]))
       (into {})))

(defn score
  [hand]
  (let [f (frequencies hand)]
    (case (sort (vals f))
      [5] 0
      [1 4] 1
      [2 3] 2
      [1 1 3] 3
      [1 2 2] 4
      [1 1 1 2] 5
      [1 1 1 1 1] 6)))

(defn parse
  [lines]
  (->> lines
       (map (fn [line] (s/split line #" ")))
       (map (fn [[a b]] [(->> a (map values) score)
                         (->> a (map values) vec)
                         (parse-long b)]))))

(defn part1
  [input]
  (->> input
       sort
       reverse
       (map-indexed (fn [idx [_ _ bid]]
                      (* (inc idx) bid)))
       (reduce + 0)))


(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 6440
  [part1 puzzle] 248569531
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
