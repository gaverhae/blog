(ns t.day14
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (map (fn [p] (map ->long (string/split p #",")))
                   (string/split line #" -> " ))))))

(defn part1
  [input]
  (let [grid (->> input
                  (mapcat (fn [line] (partition 2 1 line)))
                  (mapcat (fn [[[x1 y1] [x2 y2]]]
                            (for [x (range (min x1 x2) (inc (max x1 x2)))
                                  y (range (min y1 y2) (inc (max y1 y2)))]
                              [x y])))
                  (reduce (fn [acc p]
                            (assoc acc p :rock))
                          {}))
        max-y (reduce (fn [acc [_ y]] (max acc y)) 0 (keys grid))]
    (loop [i 0
           grid grid]
      (let [next-grid (loop [[x y] [500 0]]
                        (cond (>= y max-y)
                              :stop
                              (nil? (get grid [x (inc y)]))
                              (recur [x (inc y)])
                              (nil? (get grid [(dec x) (inc y)]))
                              (recur [(dec x) (inc y)])
                              (nil? (get grid [(inc x) (inc y)]))
                              (recur [(inc x) (inc y)])
                              :else
                              (assoc grid [x y] :sand)))]
        (if (= next-grid :stop)
          i
          (recur (inc i) next-grid))))))

(defn part2
  [input]
  (let [grid (->> input
                  (mapcat (fn [line] (partition 2 1 line)))
                  (mapcat (fn [[[x1 y1] [x2 y2]]]
                            (for [x (range (min x1 x2) (inc (max x1 x2)))
                                  y (range (min y1 y2) (inc (max y1 y2)))]
                              [x y])))
                  (reduce (fn [acc p]
                            (assoc acc p :rock))
                          {}))
        max-y (+ 2 (reduce (fn [acc [_ y]] (max acc y)) 0 (keys grid)))
        lookup (fn [grid [x y]]
                 (if (== y max-y)
                   :rock
                   (get grid [x y])))]
    (loop [i 0
           grid grid]
      (if (= :sand (get grid [500 0]))
        i
        (let [next-grid (loop [[x y] [500 0]]
                          (cond (nil? (lookup grid [x (inc y)]))
                                (recur [x (inc y)])
                                (nil? (lookup grid [(dec x) (inc y)]))
                                (recur [(dec x) (inc y)])
                                (nil? (lookup grid [(inc x) (inc y)]))
                                (recur [(inc x) (inc y)])
                                :else
                                (assoc grid [x y] :sand)))]
          (recur (inc i) next-grid))))))


(lib/check
  parse
  part1 24 799
  part2 93 29076
  )
