(ns t.day17
  (:require [clojure.set :as set]))

(defn parse
  [lines]
  (->> lines
       (map-indexed (fn [y line] (->> line (keep-indexed (fn [x c] (when (= c \#) [x y 0]))))))
       (apply concat)
       set))

(defn neighbourhood
  [v]
  (reduce (fn [n idx]
            (set (mapcat (fn [t] [t (update t idx inc) (update t idx dec)]) n)))
          #{v}
          (range (count v))))

(defn neighbours
  [v]
  (disj (neighbourhood v) v))

(defn part1
  [input]
  (->> (reduce (fn [prev _]
                 (let [[min-x max-x min-y max-y min-z max-z]
                       (reduce (fn [[min-x max-x min-y max-y min-z max-z] [x y z]]
                                 [(min min-x (dec x))
                                  (max max-x (inc x))
                                  (min min-y (dec y))
                                  (max max-y (inc y))
                                  (min min-z (dec z))
                                  (max max-z (inc z))])
                               [0 0 0 0 0 0]
                               prev)]
                   (set (for [x (range min-x (inc max-x))
                              y (range min-y (inc max-y))
                              z (range min-z (inc max-z))
                              :let [active? (prev [x y z])
                                    active-neighbours (count (set/intersection (neighbours [x y z]) prev))]
                              :when (or (and active? (#{2 3} active-neighbours))
                                        (and (not active?) (= 3 active-neighbours)))]
                          [x y z]))))
               input
               (range 6))
       count))

(defn part2
  [input]
  (->> (reduce (fn [prev _]
                 (let [[min-x max-x min-y max-y min-z max-z min-w max-w]
                       (reduce (fn [[min-x max-x min-y max-y min-z max-z min-w max-w] [x y z w]]
                                 [(min min-x (dec x))
                                  (max max-x (inc x))
                                  (min min-y (dec y))
                                  (max max-y (inc y))
                                  (min min-z (dec z))
                                  (max max-z (inc z))
                                  (min min-w (dec w))
                                  (max max-w (inc w))])
                               [0 0 0 0 0 0 0 0]
                               prev)]
                   (set (for [x (range min-x (inc max-x))
                              y (range min-y (inc max-y))
                              z (range min-z (inc max-z))
                              w (range min-w (inc max-w))
                              :let [active? (prev [x y z w])
                                    active-neighbours (count (set/intersection (neighbours [x y z w]) prev))]
                              :when (or (and active? (#{2 3} active-neighbours))
                                        (and (not active?) (= 3 active-neighbours)))]
                          [x y z w]))))
               (set (map (fn [[x y z]] [x y z 0]) input))
               (range 6))
       count))
