(ns t.day03
  (:require [clojure.set :as set]
            [t.lib :as lib]))

(defn parse
  [input]
  (->> input))

(defn part1
  [input]
  (->> input
       (map (fn [line] (let [s (count line)]
                         [(set (take (/ s 2) line))
                          (set (drop (/ s 2) line))])))
       (map (fn [[a b]] (set/intersection a b)))
       (map first)
       (map (fn [c] (if (<= (int \a) (int c) (int \z))
                      (+ 1 (- (int c) (int \a)))
                      (+ 27 (- (int c) (int \A))))))
       (reduce + 0)))

(defn part2
  [input]
  (->> input
       (partition 3)
       (map (fn [[l1 l2 l3]]
              (set/intersection (set l1) (set l2) (set l3))))
       (map first)
       (map (fn [c] (if (<= (int \a) (int c) (int \z))
                      (+ 1 (- (int c) (int \a)))
                      (+ 27 (- (int c) (int \A))))))
       (reduce + 0)))

(lib/check
  parse
  part1 157 7716
  part2 70 2973
  )
