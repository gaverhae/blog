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

(defn part1
  [input]
  (->> input
       (reduce (fn [{:keys [head tail visited]} direction]
                 (let [head [(+ (head 0) (direction 0))
                             (+ (head 1) (direction 1))]
                       tail (follow head tail)]
                   {:head head, :tail tail, :visited (conj visited tail)}))
               {:head [0 0], :tail [0 0], :visited #{[0 0]}})
       :visited
       count))

(defn part2
  [input]
  (->> input
       (reduce (fn [{:keys [head tails visited]} direction]
                 (let [head [(+ (head 0) (direction 0))
                             (+ (head 1) (direction 1))]
                       tails (rest (reductions follow head tails))]
                   {:head head, :tails tails, :visited (conj visited (last tails))}))
               {:head [0 0], :tails (repeat 9 [0 0]), :visited #{[0 0]}})
       :visited
       count))

(lib/check
  [part1 sample] 13
  [part1 puzzle] 6212
  [part2 sample] 1
  [part2 puzzle] 2522)
