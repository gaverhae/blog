(ns t.day03
  (:require [clojure.set :as set]
            [t.lib :as lib]))

(defn solve
  [groups]
  (->> groups
       (map (fn [s] (->> s (map set) (apply set/intersection) first)))
       (map (fn [c] (if (<= (int \a) (int c) (int \z))
                      (+ 1 (- (int c) (int \a)))
                      (+ 27 (- (int c) (int \A))))))
       (reduce + 0)))

(defn part1
  [input]
  (->> input
       (map (fn [line] (let [s (count line)]
                         [(take (/ s 2) line)
                          (drop (/ s 2) line)])))
       solve))

(defn part2
  [input]
  (->> input
       (partition 3)
       solve))

(lib/check
  identity
  part1 157 7716
  part2 70 2973)
