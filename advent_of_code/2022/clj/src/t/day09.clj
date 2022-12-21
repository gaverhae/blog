(ns t.day09
  (:require [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (mapcat (fn [line]
                 (let [[_ direction distance] (re-matches #"(\S) (\d+)" line)]
                   (repeat (->long distance)
                           ({"U" [1 0], "D" [-1 0], "L" [0 -1], "R" [0 1]}
                            direction)))))))

(defn follow
  [[y0 x0] [y x]]
  (let [dy ^long (- y0 y)
        dx ^long (- x0 x)
        sgn (fn [d] (cond (pos? d) 1 (neg? d) -1 (zero? d) 0))]
    (cond (and (<= (Math/abs dy) 1)
               (<= (Math/abs dx) 1)) [y x]
          :else [(+ (sgn dy) y) (+ (sgn dx) x)])))

(defn solve
  [input num-tails]
  (->> input
       (reduce (fn [[[y x] tails visited] [dy dx]]
                 (let [head [(+ y dy) (+ x dx)]
                       tails (rest (reductions follow head tails))]
                   [head tails (conj visited (last tails))]))
               [[0 0] (repeat num-tails [0 0]) #{[0 0]}])
       last
       count))

(defn part1
  [input]
  (solve input 1))

(defn part2
  [input]
  (solve input 9))

(lib/check
  [part1 sample] 13
  [part1 puzzle] 6212
  [part2 sample] 1
  [part2 puzzle] 2522)
