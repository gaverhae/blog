(ns t.day08
  (:require [t.lib :as lib :refer [l]]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]))

(defn parse
  [lines]
  (->> lines
       (mapv (fn [line] (mapv (fn [c] (l (str c))) line)))))

(defn part1
  [input]
  (let [h (count input)
        w (count (first input))
        tinput (lib/transpose input)
        visible (for [y (range 1 (dec h))
                      x (range 1 (dec w))
                      :let [v (get-in input [y x])
                            left (take x (get input y))
                            right (drop (inc x) (get input y))
                            above (take y (get tinput x))
                            below (drop (inc y) (get tinput x))]
                      :when (or (> v (apply max left))
                                (> v (apply max right))
                                (> v (apply max above))
                                (> v (apply max below)))]
                  [x y])
        border (- (* 2 (+ h w)) 4)]
    (+ (count visible) border)))

(defn part2
  [input]
  (let [h (count input)
        w (count (first input))
        tinput (lib/transpose input)
        scenic (for [y (range 1 (dec h))
                      x (range 1 (dec w))
                      :let [v (get-in input [y x])
                            f (fn f [s] (cond (empty? s) ()
                                              (< (first s) v) (cons (first s) (f (rest s)))
                                              :else [(first s)]))
                            left (count (f (reverse (take x (get input y)))))
                            right (count (f (drop (inc x) (get input y))))
                            above (count (f (reverse (take y (get tinput x)))))
                            below (count (f (drop (inc y) (get tinput x))))]]
                  (* left right above below))]
    (apply max scenic)))

(lib/check
  parse
  part1 21 1533
  part2 8 345744)
