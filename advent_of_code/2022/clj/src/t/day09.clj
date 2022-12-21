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
  [head  tail]
  (case [(- (head 0) (tail 0))
         (- (head 1) (tail 1))]
    [1 -1] tail
    [1 0] tail
    [1 1] tail
    [0 -1] tail
    [0 0] tail
    [0 1] tail
    [-1 -1] tail
    [-1 0] tail
    [-1 1] tail
    [1 -2] [(inc (tail 0)) (dec (tail 1))]
    [2 -1] [(inc (tail 0)) (dec (tail 1))]
    [2 0] [(inc (tail 0)) (tail 1)]
    [1 2] [(inc (tail 0)) (inc (tail 1))]
    [2 1] [(inc (tail 0)) (inc (tail 1))]
    [0 2] [(tail 0) (inc (tail 1))]
    [-1 2] [(dec (tail 0)) (inc (tail 1))]
    [-2 1] [(dec (tail 0)) (inc (tail 1))]
    [-2 0] [(dec (tail 0)) (tail 1)]
    [-2 -1] [(dec (tail 0)) (dec (tail 1))]
    [-1 -2] [(dec (tail 0)) (dec (tail 1))]
    [0 -2] [(tail 0) (dec (tail 1))]
    [2 2] [(inc (tail 0)) (inc (tail 1))]
    [-2 2] [(dec (tail 0)) (inc (tail 1))]
    [2 -2] [(inc (tail 0)) (dec (tail 1))]
    [-2 -2] [(dec (tail 0)) (dec (tail 1))]))

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
