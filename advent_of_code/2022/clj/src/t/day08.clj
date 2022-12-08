(ns t.day08
  (:require [t.lib :as lib :refer [->long]]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]))

(defn parse
  [lines]
  (let [grid (mapv #(mapv (comp ->long str) %) lines)
        tgrid (lib/transpose grid)]
    (for [y (range (count grid))
          x (range (count (first grid)))]
      {:value (get-in grid [y x])
       :left (reverse (take x (get grid y)))
       :right (drop (inc x) (get grid y))
       :above (reverse (take y (get tgrid x)))
       :below (drop (inc y) (get tgrid x))})))

(defn part1
  [input]
  (->> input
       (filter (fn [{:keys [value left right above below]}]
                 (or (> value (apply max -1 left))
                     (> value (apply max -1 right))
                     (> value (apply max -1 above))
                     (> value (apply max -1 below)))))
       count))

(defn part2
  [input]
  (->> input
       (map (fn [{:keys [value left right above below]}]
              (let [f (fn f [s] (cond (empty? s) ()
                                      (< (first s) value) (cons (first s) (f (rest s)))
                                      :else [(first s)]))]
                (* (count (f left))
                   (count (f right))
                   (count (f above))
                   (count (f below))))))
       (apply max)))

(lib/check
  parse
  part1 21 1533
  part2 8 345744)
