(ns t.day5
  (:import [java.util BitSet])
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [t.util :refer [transpose]]))

(defn sign
  [x]
  (if (pos? x) 1 (if (neg? x) -1 0)))

(defn count-overlaps
  [segments]
  (let [one (BitSet. (* 1000 1000))
        two (BitSet. (* 1000 1000))]
    (doseq [[x1 y1 x2 y2] segments]
      (let [dx (- x2 x1)
            dy (- y2 y1)
            sign-dx (sign dx)
            sign-dy (sign dy)
            num-steps (max (* sign-dx dx)
                           (* sign-dy dy))]
        (dotimes [n (inc num-steps)]
          (let [idx (int (+ (* 1000 (+ x1 (* n sign-dx)))
                            (+ y1 (* n sign-dy))))]
            (when-not (.get two idx)
              (if (.get one idx)
                (.set two idx)
                (.set one idx)))))))
    (.cardinality two)))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (->> (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)
                   rest
                   (map #(Long/parseLong %)))))))

(defn part1
  [input]
  (->> input
       (filter (fn [[x1 y1 x2 y2]]
                 (or (= x1 x2) (= y1 y2))))
       count-overlaps))

(defn part2
  [input]
  (->> input
       count-overlaps))
