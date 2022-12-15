(ns t.day12
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn dijkstra-search
  [initial final? generate-moves]
  (let [to-visit (java.util.PriorityQueue. 100 compare)]
    (loop [[cost state] [0 initial]
           visited #{}]
      (when (not (visited state))
        (doseq [[nxt-state nxt-cost] (generate-moves [state cost])]
          (when (not (visited nxt-state))
            (.add to-visit [nxt-cost nxt-state]))))
      (if (final? state)
        cost
        (recur (.poll to-visit)
               (conj visited state))))))

(defn parse
  [lines]
  (->> lines
       (mapv (fn [line] (mapv (fn [c] (long c)) line)))))

(defn neighbours
  [[x y]]
  #{[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]})

(defn height
  [input pos]
  (when-let [h (get-in input pos)]
    (cond (== h (long \S)) (long \a)
          (== h (long \E)) (long \z)
          :else h)))

(defn part1
  [input]
  (dijkstra-search
    (loop [j 0
           i 0]
      (cond (== i (count (first input))) (recur (inc j) 0)
            (== (long \S) (get-in input [j i])) [j i]
            :else (recur j (inc i))))
    (fn [p] (when-let [s (get-in input p)]
              (== (long \E) s)))
    (fn [[pos cost-so-far]]
      (->> (neighbours pos)
           (keep (fn [p] (when-let [c (height input p)]
                           (when (>= 1 (- (height input p)
                                          (height input pos)))
                             [p (inc cost-so-far)]))))
           set))))

(defn part2
  [input]
  (dijkstra-search
    (loop [j 0
           i 0]
      (cond (== i (count (first input))) (recur (inc j) 0)
            (== (long \E) (get-in input [j i])) [j i]
            :else (recur j (inc i))))
    (fn [p] (when-let [s (get-in input p)]
              (== (long \a) s)))
    (fn [[pos cost-so-far]]
      (->> (neighbours pos)
           (keep (fn [p] (when-let [c (height input p)]
                           (when (>= 1 (- (height input pos)
                                          (height input p)))
                             [p (inc cost-so-far)]))))
           set))))

(lib/check
  [part1 sample] 31
  [part1 puzzle] 383
  [part2 sample] 29
  [part2 puzzle] 377)
