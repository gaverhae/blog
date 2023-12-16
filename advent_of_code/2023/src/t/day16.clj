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
  (loop [ps [[[0 0] [0 1]]]
         energized #{[0 0]}
         seen? #{}]
    (print (str (char 27) "[2J")) ; clear screen
    (print (str (char 27) "[;H")) ; move cursor to the top left corner of the screen
    (->> input
         (map-indexed
           (fn [y line]
             (->> line
                  (map-indexed
                    (fn [x ch]
                      (cond (= (ffirst ps) [y x]) "\033[31m#\033[0m"
                            (some (fn [[p _]] (= p [y x])) ps) (str "\033[32m" ch "\033[0m")
                            (and (= ch \.) (energized [y x])) "\033[33m•\033[0m"
                            (energized [y x]) (str "\033[33m" ch "\033[0m")
                            (= ch \.) \space
                            :else ch)))
                  (apply str)
                  println)))
         doall)
    (read-line)
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
  [part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
