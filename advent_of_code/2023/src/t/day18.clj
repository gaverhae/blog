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
  (let [verticals (->> lines
                       (reduce (fn [[y0 x0 verts] [[dy dx] dist]]
                                 (let [y1 (+ y0 (* dy dist))
                                       x1 (+ x0 (* dx dist))]
                                   [y1 x1 (if (zero? dx)
                                            (conj verts [x0 y0 y1])
                                            verts)]))
                               [0 0 []])
                       (drop 2)
                       first
                       sort
                       (map (fn [[x y0 y1]]
                              (if (< y0 y1)
                                [x y0 y1 false]
                                [x y1 y0 true]))))
        entering (->> verticals (filter #(get % 3)))
        exiting (->> verticals (remove #(get % 3)))]
    (loop [entering entering
           p []
           cells 0]
      (if (empty? entering)
        [cells p]
        (let [[[x y0 y1] & entering] entering
              exits (->> exiting
                         (filter (fn [[e-x e-y0 e-y1]]
                                   (and (> e-x x)
                                        (or (<= e-y0 y0 e-y1 y1)
                                            (<= y0 e-y0 e-y1 y1)
                                            (<= e-y0 y0 y1 e-y1)
                                            (<= y0 e-y0 y1 e-y1)))))
                         (map (fn [[e-x e-y0 e-y1]]
                                [e-x (max e-y0 y0) (min e-y1 y1)])))
              ys (->> exits
                      (mapcat (fn [[e-x e-y0 e-y1]] [e-y0 e-y1]))
                      set sort)
              intervals (reduce (fn [acc el]
                                  (prn [:acc acc :el el])
                                  (conj acc [(inc (second (last acc))) el]))
                                [(vec (take 2 ys))]
                                (drop 2 ys))
              new-cells (->> intervals
                             (map (fn [[i-y0 i-y1]]
                                    (let [x-end (->> exits
                                                     (filter (fn [[e-x e-y0 e-y1]]
                                                               (or (<= e-y0 i-y0 e-y1 i-y1)
                                                                   (<= i-y0 e-y0 e-y1 i-y1)
                                                                   (<= e-y0 i-y0 i-y1 e-y1)
                                                                   (<= i-y0 e-y0 i-y1 e-y1))))
                                                     first first)
                                          width (inc (- x-end x))
                                          height (inc (- i-y1 i-y0))]
                                      (* width height))))
                             (reduce +))]
          (recur entering
                 (conj p [[x y0 y1 new-cells]])
                 0))))))


(defn part1
  [input]
  (solve (:part1 input)))

(defn part2
  [input]
  (solve (:part2 input)))

(lib/check
  [part1 sample] 62
  #_#_[part1 puzzle] 48503
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
