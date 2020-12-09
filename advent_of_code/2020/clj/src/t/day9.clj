(ns t.day9
  (:require [clojure.set :as set]))

(defn parse
  [lines]
  (mapv #(Long/parseLong %) lines))

(defn part1
  [preamble input]
  (let [pre (take preamble input)
        valid (set (for [x (range preamble)
                         y (range x preamble)]
                     (+ (input x) (input y))))
        nex (input preamble)]
    (if (valid nex)
      (recur preamble (subvec input 1))
      nex)))

(defn part2
  [preamble input]
  (let [target (part1 preamble input)]
    (loop [start 0
           size 2]
      (let [span (take size (drop start input))
            sum (reduce + span)]
        (cond (< sum target) (recur start (inc size))
              (= sum target) (+ (apply max span) (apply min span))
              (> sum target) (recur (inc start) 2))))))
