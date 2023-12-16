(ns t.day16
  (:require [clojure.core.async :as async]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  (vec lines))

(defn step
  [dy dx c]
  (let [left [0 -1], right [0 1], up [-1 0], down [1 0]
        m {[left \.] [left]
           [left \\] [up]
           [left \/] [down]
           [left \|] [up down]
           [left \-] [left]
           [right \.] [right]
           [right \\] [down]
           [right \/] [up]
           [right \|] [up down]
           [right \-] [right]
           [up \.] [up]
           [up \\] [left]
           [up \/] [right]
           [up \|] [up]
           [up \-] [left right]
           [down \.] [down]
           [down \\] [right]
           [down \/] [left]
           [down \|] [down]
           [down \-] [left right]}]
    (get m [[dy dx] c])))

(defn part1
  [input]
  (loop [ps [[[0 -1] [0 1]]]
         energized #{}
         seen? #{}]
    (cond (empty? ps) (count energized)
          (seen? (first ps)) (recur (rest ps) energized seen?)
          :else (let [p (first ps), ps (rest ps)
                      seen? (conj seen? p)
                      [[y x] [dy dx]] p
                      nxt [(+ y dy) (+ x dx)]]
                  (if-let [c (get-in input nxt)]
                    (recur (reduce (fn [acc el] (conj acc [nxt el])) ps (step dy dx c))
                           (conj energized nxt) seen?)
                    (recur ps energized seen?))))))

(defn part2
  [input]
  (->> input))

(lib/check
  #_#_[part1 sample] 46
  [part1 puzzle] 7951
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
