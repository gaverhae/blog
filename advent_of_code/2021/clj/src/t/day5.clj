(ns t.day5
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [t.util :refer [transpose]]))

(defn sign
  [x]
  (cond (zero? x) 0
        (pos? x) 1
        (neg? x) -1))

(defn count-overlaps
  [segments]
  (let [arr (int-array (* 1000 1000))]
    (doseq [[x1 y1 x2 y2] segments]
      (let [dx (- x2 x1)
            dy (- y2 y1)
            sign-dx (sign dx)
            sign-dy (sign dy)
            num-steps (max (* sign-dx dx)
                           (* sign-dy dy))]
        (dotimes [n (inc num-steps)]
          (let [idx (+ (* 1000 (+ x1 (* n sign-dx)))
                       (+ y1 (* n sign-dy)))]
            (aset arr idx (inc (aget arr idx)))))))
    (->> arr
         (filter (fn [x] (>= x 2)))
         count)))

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
