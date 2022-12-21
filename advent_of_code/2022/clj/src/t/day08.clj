(ns t.day08
  (:require [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (let [grid (mapv #(mapv (comp ->long str) %) lines)
        tgrid (lib/transpose grid)]
    (for [y (range (count grid))
          x (range (count (first grid)))]
      {:value (get-in grid [y x])
       :views [(reverse (take x (get grid y)))
               (drop (inc x) (get grid y))
               (reverse (take y (get tgrid x)))
               (drop (inc y) (get tgrid x))]})))

(defn part1
  [input]
  (->> input
       (filter (fn [{:keys [value views]}]
                 (some #(> value (apply max -1 %)) views)))
       count))

(defn part2
  [input]
  (->> input
       (map (fn [{:keys [value views]}]
              (let [f (fn f [s] (cond (empty? s) ()
                                      (< (first s) value) (cons (first s) (f (rest s)))
                                      :else [(first s)]))]
                (->> views (map (comp count f)) (reduce *)))))
       (apply max)))

(lib/check
  [part1 sample] 21
  [part1 puzzle] 1533
  [part2 sample] 8
  [part2 puzzle] 345744)
