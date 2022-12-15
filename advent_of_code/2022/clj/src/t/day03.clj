(ns t.day03
  (:require [clojure.set :as set]
            [t.lib :as lib]))

(def parse identity)

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
  [part1 sample] 157
  [part1 puzzle] 7716
  [part2 sample] 70
  [part2 puzzle] 2973)
