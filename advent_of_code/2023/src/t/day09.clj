(ns t.day09
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (map parse-long (re-seq #"-?\d+" line))))))

(defn part1
  [input]
  (->> input
       (map (fn [line]
              line
              (loop [seqs [line]]
                (if (every? zero? (first seqs))
                  (reduce (fn [acc el]
                            (+ acc (last el)))
                          (last (first seqs))
                          (rest seqs))
                  (recur (cons (->> (first seqs)
                                    (partition 2 1)
                                    (map (fn [[a b]] (- b a))))
                               seqs))))))
       (reduce + 0)))

(defn part2
  [input]
  (->> input
       (map (fn [line]
              line
              (loop [seqs [line]]
                (if (every? zero? (first seqs))
                  (reduce (fn [acc el]
                            (- el acc))
                          (first (first seqs))
                          (map first (rest seqs)))
                  (recur (cons (->> (first seqs)
                                    (partition 2 1)
                                    (map (fn [[a b]] (- b a))))
                               seqs))))))
       (reduce + 0)))

(lib/check
  [part1 sample] 114
  [part1 puzzle] 1953784198
  [part2 sample] 2
  [part2 puzzle] 957)
