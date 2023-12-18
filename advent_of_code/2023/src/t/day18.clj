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
                                  [y x1 x0]))))
        interesting-ys (->> verticals
                            (mapcat (fn [[_ y0 y1 _]] [y0 (inc y0) y1 (inc y1)]))
                            set
                            sort)]
    (let [trench (loop [to-dig (->> lines
                                    (mapcat (fn [[dir dist]] (repeat dist dir))))
                        [y x] [0 0]
                        [py px] [0 1]
                        trench {[0 0] \F}]
                   (if (empty? to-dig)
                     (assoc trench [y x] \F)
                     (let [[[dy dx] & to-dig] to-dig]
                       (recur to-dig
                              [(+ y dy) (+ x dx)]
                              [y x]
                              (assoc trench [y x] (match [(- y py) (- x px) dy dx]
                                                    [ 0  1  1  0] \7
                                                    [-1  0  0 -1] \7
                                                    [-1  0  0  1] \F
                                                    [ 0 -1  1  0] \F
                                                    [ _  0  _  0] \|
                                                    [ 0  _  0  _] \-
                                                    [ 1  0  0  1] \L
                                                    [ 0 -1 -1  0] \L
                                                    [ 0  1 -1  0] \J
                                                    [ 1  0  0 -1] \J))))))
          min-y (->> trench keys (map first) (reduce min))
          max-y (->> trench keys (map first) (reduce max) inc)
          min-x (->> trench keys (map second) (reduce min))
          max-x (->> trench keys (map second) (reduce max) inc)
          dug (->> interesting-ys
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
                   (partition 2 1)
                   (mapcat (fn [[[y0 xs] [y1 _]]]
                             (let [height (- y1 y0)]
                               (->> (range height)
                                    (mapcat (fn [dy]
                                              (->> xs
                                                   (mapcat (fn [[x0 x1]]
                                                             (->> (range (inc (- x1 x0)))
                                                                  (map (fn [dx] [(+ y0 dy) (+ x0 dx)]))))))))))))
                   (into #{}))
          drawing (->> (range min-y max-y)
                       (map (fn [y]
                              (->> (range min-x max-x)
                                   (map (fn [x]
                                          (let [c (trench [y x] \space)]
                                            (if (dug [y x]) (lib/bg-color :cyan c) c))))
                                   (apply str)))))]
      (println)
      (->> drawing (map println) doall)
      (println))
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
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
