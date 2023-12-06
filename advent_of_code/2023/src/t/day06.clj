(ns t.day06
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line] (map parse-long (re-seq #"\d+" line))))
       lib/transpose))

(defn part1
  [input]
  (->> input
       (map (fn [[t d]]
              (for [press (range t)
                    :let [traveled (* press (- t press))]
                    :when (> traveled d)]
                [press traveled])))
       (map count)
       (reduce * 1)))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 288
  [part1 puzzle] 608902
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
