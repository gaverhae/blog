(ns t.day6
  (:require [clojure.string :as string]))

(defn parse
  [lines]
  (-> lines first (string/split #",") (->> (map #(Long/parseLong %)))))

(defn part1
  [input]
  (->> input
       frequencies
       (iterate (fn [ages]
                  (apply merge-with +
                              (map (fn [[age n]]
                                     (if (zero? age)
                                       {8 n 6 n}
                                       {(dec age) n}))
                                   ages))))
       (drop 80)
       first
       vals
       (reduce +)))

(defn part2
  [input]
  (->> input
       frequencies
       (iterate (fn [ages]
                  (apply merge-with +
                              (map (fn [[age n]]
                                     (if (zero? age)
                                       {8 n 6 n}
                                       {(dec age) n})) ages))))
       (drop 256)
       first
       vals
       (reduce +)))
