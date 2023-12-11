(ns t.day11
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(defn parse
  [lines]
  {:empty-lines (->> lines
                     (keep-indexed
                       (fn [y line]
                         (when (every? #{\.} line) y))))
   :empty-cols (->> lines
                    lib/transpose
                    (keep-indexed
                      (fn [x col]
                        (when (every? #{\.} col) x))))
   :base-coords (->> lines
                     (keep-indexed
                       (fn [y line]
                         (keep-indexed
                           (fn [x c]
                             (when (= c \#)
                               [y x]))
                           line)))
                     (apply concat))})

(defn solve
  [{:keys [empty-lines empty-cols base-coords]} expand]
  (let [adjusted (->> base-coords
                      (map (fn [[y x]]
                             [(->> empty-lines
                                   (filter #(< % y))
                                   count
                                   (* (dec expand))
                                   (+ y))
                              (->> empty-cols
                                   (filter #(< % x))
                                   count
                                   (* (dec expand))
                                   (+ x))])))]
    (->> (for [g1 adjusted
               g2 adjusted
               :when (and (= 1 (compare g1 g2))
                          (not= g1 g2))]
           (lib/manhattan g1 g2))
         (reduce + 0))))

(defn part1
  [input]
  (solve input 2))

(defn part2
  [input expand]
  (solve input expand))

(lib/check
  [part1 sample] 374
  [part1 puzzle] 9947476
  [part2 sample 10] 1030
  [part2 sample 100] 8410
  [part2 puzzle 1000000] 519939907614)
