(ns t.day5
  (:require [clojure.string :as string]))

(defn parse
  [lines]
  (map (fn [line]
         (reduce (fn [acc el]
                   (+ (* 2 acc)
                      ({\F 0, \B 1, \L 0, \R 1} el)))
                 0
                 line))
       lines))

(defn part1
  [input]
  (apply max input))

(defn part2
  [input]
  (let [[before after] (->> (sort input)
                            (partition 2 1)
                            (filter (fn [[p n]] (= 2 (- n p))))
                            first)]
    (/ (+ before after)
       2)))
