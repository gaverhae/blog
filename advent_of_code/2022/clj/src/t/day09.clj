(ns t.day09
  (:require [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [[_ direction distance] (re-matches #"(\S) (\d+)" line)]
                [(keyword direction) (->long distance)])))))

(defn part1
  [input]
  (->> input
       (mapcat (fn [[d n]] (repeat n d)))
       (map {:U [1 0], :D [-1 0], :L [0 -1], :R [0 1]})
       (reduce (fn [{:as acc :keys [head tail visited]} direction]
                 (let [head [(+ (head 0) (direction 0))
                             (+ (head 1) (direction 1))]
                       tail (case [(- (head 0) (tail 0))
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
                              [0 -2] [(tail 0) (dec (tail 1))])]
                   {:head head, :tail tail, :visited (conj visited tail)}))
                   {:head [0 0], :tail [0 0], :visited #{[0 0]}})
       :visited
       count))

(defn part2
  [input]
  )

(lib/check
  parse
  part1 13 0
  ;part2 8 345744
  )
