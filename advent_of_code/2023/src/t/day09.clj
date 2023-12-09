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
              (map parse-long (re-seq #"-?\d+" line))))
       (map (fn [line]
              (loop [seqs [line]]
                (if (every? zero? (first seqs))
                  seqs
                  (recur (cons (->> (first seqs)
                                    (partition 2 1)
                                    (map (fn [[a b]] (- b a))))
                               seqs))))))))

(defn solve
  [op select]
  (fn [input]
    (->> input
         (map (fn [seqs]
                (reduce (fn [acc el]
                          (op el acc))
                        (select (first seqs))
                        (map select (rest seqs)))))
         (reduce + 0))))

(def part1
  (solve + last))

(def part2
  (solve - first))

(lib/check
  [part1 sample] 114
  [part1 puzzle] 1953784198
  [part2 sample] 2
  [part2 puzzle] 957)
