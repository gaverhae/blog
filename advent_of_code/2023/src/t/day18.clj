(ns t.day18
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  (let [lines (->> lines
                   (map (fn [line]
                          (let [[dir dist color] (s/split line #" ")
                                dir (get {"R" [0 1], "D" [1 0], "L" [0 -1], "U" [-1 0]} dir)
                                dist (parse-long dist)
                                color (re-find #"[0-9a-f]{6}" color)
                                dir2 (get {\0 [0 1], \1 [1 0], \2 [0 -1], \3 [-1 0]}
                                          (get color 5))
                                dist2 (Long/parseLong (subs color 0 5) 16)]
                            [dir dist [dir2 dist2]]))))]
    {:part1 (->> lines (map (fn [[dir dist _]] [dir dist])))
     :part2 (->> lines (map (fn [[_ _ [dir dist]]] [dir dist])))}))

(defn solve
  [lines]
  (let [verticals (reduce (fn [acc [dir dist]]
                            acc)
                          [[0 0] []]
                          lines)]
    verticals))


(defn part1
  [input]
  (solve (:part1 input)))

(defn part2
  [input]
  (solve (:part2 input)))

(lib/check
  [part1 sample] 62
  #_#_[part1 puzzle] 48503
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
