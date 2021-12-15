(ns t.day15
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn parse
  [lines]
  (->> lines
       (mapv (fn [line]
              (mapv (fn [c] (Long/parseLong (str c))) line)))))

(defn neighbours
  [[x y]]
  #{[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]})

(defn part1
  [input]
  (let [cost-of-entering (->> input
                              (map-indexed (fn [y line]
                                             (map-indexed (fn [x cost]
                                                            [[x y] cost])
                                                          line)))
                              (apply concat)
                              (into {}))
        target [(dec (count (first input)))
                (dec (count input))]]
    (loop [[cost pos] [0 [0 0]]
           unvisited (dissoc (->> cost-of-entering
                                  (map (fn [[k v]] [k Long/MAX_VALUE]))
                                  (into {}))
                             [0 0])]
      (if (= pos target)
        (unvisited pos)
        (let [new-unvisited (reduce (fn [unvisited neighbour]
                                      (update unvisited
                                              neighbour
                                              (fn [old]
                                                (min old (+ cost
                                                            (cost-of-entering neighbour))))))
                                    (dissoc unvisited pos)
                                    (->> (neighbours pos)
                                         (filter unvisited)))]
          (recur (->> new-unvisited
                      (map (fn [[k v]] [v k]))
                      sort
                      first)
                 new-unvisited))))))

(defn part2
  [input])
