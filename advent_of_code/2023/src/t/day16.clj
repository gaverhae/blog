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

(defn solve
  [input start]
  (loop [ps [start]
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

(defn part1
  [input]
  (solve input [[0 -1] [0 1]]))

(defn part2
  [input]
  (let [h (count input)
        w (count (first input))]
    (->> (concat (->> (range h) (map (fn [y] [[ y -1] [ 0  1]])))  ;; from the left
                 (->> (range h) (map (fn [y] [[ y  w] [ 0 -1]])))  ;; from the right
                 (->> (range w) (map (fn [x] [[-1  x] [ 1  0]])))  ;; from the top
                 (->> (range w) (map (fn [x] [[ h  x] [-1  0]])))) ;; from the bottom
         (map (fn [start] (solve input start)))
         (reduce max))))

(lib/check
  [part1 sample] 46
  [part1 puzzle] 7951
  [part2 sample] 51
  [part2 puzzle] 8148)
