(ns t.day18
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  (let [lines (->> lines
                   (map (fn [line]
                          (let [[dir dist color] (s/split line #" ")
                                dir (get {"R" [0 1], "D" [1 0], "L" [0 -1], "U" [-1 0]} dir)
                                dist (parse-long dist)
                                color (re-find #"[0-9a-f]{6}" color)
                                dir2 (get {\0 [0 1], \1 [1 0], \2 [0 -1], \3 [-1 0]}
                                          (get color 5))
                                dist2 (Long/parseLong (subs color 0 5) 16)]
                            [dir dist [dir2 dist2]]))))]
    {:part1 (->> lines (map (fn [[dir dist _]] [dir dist])))
     :part2 (->> lines (map (fn [[_ _ [dir dist]]] [dir dist])))}))

(defn solve
  [lines]
  (let [path (->> lines
                  (reduce (fn [[y0 x0 vs hs] [[dy dx] dist]]
                            (let [y1 (+ y0 (* dy dist))
                                  x1 (+ x0 (* dx dist))]
                              [y1
                               x1
                               (if (zero? dx) (conj vs [x0 y0 y1]) vs)
                               (if (zero? dy) (conj hs [y0 x0 x1]) hs)]))
                          [0 0 [] []])
                  (drop 2))
        verticals (->> path
                       first
                       sort
                       (map (fn [[x y0 y1]]
                              (if (< y0 y1)
                                [x y0 y1 false]
                                [x y1 y0 true]))))
        horizontals (->> path
                         second
                         sort
                         (map (fn [[y x0 x1]]
                                (if (< x0 x1)
                                  [y x0 x1]
                                  [y x1 x0])))
                         (reduce (fn [acc [y x0 x1]]
                                   (update acc y (fnil conj #{}) [x0 x1]))
                                 {}))
        interesting-ys (->> verticals
                            (mapcat (fn [[_ y0 y1 _]] [y0 (inc y0) y1 (inc y1)]))
                            set
                            sort)]
    (->> interesting-ys
         (map (fn [y]
                [y (->> verticals
                        (filter (fn [[_ y0 y1 _]] (<= y0 y y1)))
                        (map (fn [[x _ _ in?]] [x in?])))]))
         (map (fn [[y xs]]
                [y (reduce (fn [acc el]
                             (let [done (vec (butlast acc))
                                   [x0 in0] (last acc)
                                   [x1 in1] el]
                               (cond (= in0 in1 true) (conj done [x0 true])
                                     (= in0 in1 false) (conj done [x1 false])
                                     :else (conj done [x0 in0] [x1 in1]))))
                           [(first xs)]
                           (rest xs))]))
         (map (fn [[y xs]]
                [y (->> xs (map first) (partition 2))]))
         (map (fn [[y xs]]
                [y (loop [todo (rest xs)
                          cur (first xs)
                          done []]
                     (if (empty? todo)
                       (conj done cur)
                       (let [[x0 x1] cur
                             [[x2 x3] & todo] todo]
                         (if (get-in horizontals [y [x1 x2]])
                           (recur todo [x0 x3] done)
                           (recur todo [x2 x3] (conj done cur))))))]))
         (partition 2 1)
         (map (fn [[[y-start xs] [y-end _]]]
                (let [height (- y-end y-start)]
                  (->> xs
                       (map (fn [[x0 x1]] (* (inc (- x1 x0)) height)))
                       (reduce + 0)))))
         (reduce + 0))))

(defn part1
  [input]
  (solve (:part1 input)))

(defn part2
  [input]
  (solve (:part2 input)))

(lib/check
  [part1 sample] 62
  [part1 puzzle] 48503
  [part2 sample] 952408144115
  #_#_[part2 puzzle] 0)
