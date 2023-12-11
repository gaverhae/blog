(ns t.day11
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(defn parse
  [lines]
  (->> lines
       (mapcat (fn [line]
                 (if (every? #{\.} line)
                   [line line]
                   [line])))
       lib/transpose
       (mapcat (fn [line]
                 (if (every? #{\.} line)
                   [line line]
                   [line])))
       lib/transpose
       (keep-indexed
         (fn [y line]
           (keep-indexed
             (fn [x c]
               (when (= c \#)
                 [y x]))
             line)))
       (apply concat)))

(defn part1
  [input]
  (->> (for [g1 input
             g2 input
             :when (and (= 1 (compare g1 g2))
                        (not= g1 g2))]
         (lib/manhattan g1 g2))
       (reduce + 0)))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 374
  [part1 puzzle] 9947476
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
