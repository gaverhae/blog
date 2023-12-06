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
  (let [[total-time record-distance] (->> input
                                          lib/transpose
                                          (map (fn [nums] (parse-long (apply str nums)))))
        shortest (loop [press 0]
                   (if (> (* press (- total-time press)) record-distance)
                     press
                     (recur (inc press))))
        longest (loop [press total-time]
                   (if (> (* press (- total-time press)) record-distance)
                     press
                     (recur (dec press))))]
    (inc (- longest shortest))))


(lib/check
  [part1 sample] 288
  [part1 puzzle] 608902
  [part2 sample] 71503
  [part2 puzzle] 46173809)
