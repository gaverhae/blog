(ns t.day04
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [[_ winning have] (->> (s/split line #"\||:")
                                          (map (fn [s] (map parse-long (re-seq #"\d+" s)))))]

                [winning have])))))

(defn part1
  [input]
  (->> input
       (map (fn [[w h]] (count (set/intersection (set w) (set h)))))
       (remove #{0})
       (map dec)
       (map #(long (math/pow 2 %)))
       (reduce + 0)))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 13
  [part1 puzzle] 25183
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
