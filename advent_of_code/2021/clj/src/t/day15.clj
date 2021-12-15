(ns t.day15
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [clojure.set :as set])
  (:import [java.util PriorityQueue]))

(defn parse
  [lines]
  (->> lines
       (mapv (fn [line]
              (mapv (fn [c] (Long/parseLong (str c))) line)))))

(defn neighbours
  [[x y]]
  #{[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]})

(defn shortest-path
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
                             [0 0])
           pq ^PriorityQueue (reduce (fn [^PriorityQueue pq [pos cost]]
                                       (.add pq [cost pos])
                                       pq)
                                     (PriorityQueue. (count cost-of-entering) compare)
                                     unvisited)]
      (if (= pos target)
        cost
        (let [neighs (->> (neighbours pos)
                          (filter unvisited))
              new-unvisited (reduce (fn [unvisited neighbour]
                                      (update unvisited
                                              neighbour
                                              (fn [old]
                                                (min old (+ cost
                                                            (cost-of-entering neighbour))))))
                                    (dissoc unvisited pos)
                                    neighs)]
          (doseq [n neighs]
            (.remove pq [(unvisited n) n])
            (.add pq [(new-unvisited n) n]))
          (recur (.poll pq)
                 new-unvisited
                 pq))))))

(defn part1
  [input]
  (shortest-path input))

(defn part2
  [input])
