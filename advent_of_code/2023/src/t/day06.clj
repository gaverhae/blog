(ns t.day06
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line] (re-seq #"\d+" line)))))

(defn solve-one
  [[t d]]
  (->> [[t dec] [0 inc]]
       (map (fn [[start dir]]
              (loop [press start]
                (if (> (* press (- t press)) d)
                  press
                  (recur (dir press))))))
       (apply -)
       inc))

(defn part1
  [input]
  (->> input
       (map (fn [nums] (map parse-long nums)))
       lib/transpose
       (map solve-one)
       (reduce * 1)))

(defn part2
  [input]
  (->> input
       (map (fn [nums] (parse-long (apply str nums))))
       solve-one))

(lib/check
  [part1 sample] 288
  [part1 puzzle] 608902
  [part2 sample] 71503
  [part2 puzzle] 46173809)
