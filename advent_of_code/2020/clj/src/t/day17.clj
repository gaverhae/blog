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

(defn to-check
  [active]
  (->> active
       (mapcat neighbourhood)
       set))

(defn part1
  [input]
  (->> (reduce (fn [prev _]
                 (set (for [v (to-check prev)
                            :let [active? (prev v)
                                  active-neighbours (count (set/intersection (neighbours v) prev))]
                            :when (or (and active? (#{2 3} active-neighbours))
                                      (and (not active?) (= 3 active-neighbours)))]
                        v)))
               input
               (range 6))
       count))

(defn part2
  [input]
  (part1 (set (map (fn [[x y z]] [x y z 0]) input))))
