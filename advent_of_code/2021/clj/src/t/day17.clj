(ns t.day17
  (:require [clojure.set :as set]))

(defn parse
  [lines]
  (let [[_ x1 x2 y1 y2] (re-matches #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)"
                                    (first lines))]
    {:x-min (Long/parseLong x1)
     :x-max (Long/parseLong x2)
     :y-min (Long/parseLong y1)
     :y-max (Long/parseLong y2)}))

(defn part1
  [{:keys [x-min x-max y-min y-max]}]
  (let [target-x (set (range x-min (inc x-max)))
        target-y (set (range y-min (inc y-max)))
        min-steps (->> (range 0 (inc x-max))
                       (map (fn [speed]
                              (->> [0 speed 0]
                                   (iterate (fn [[pos speed iter]]
                                              [(+ pos speed) (dec speed) (inc iter)]))
                                   (take (inc speed))
                                   last)))
                       (filter (fn [[pos _ _]]
                                 (contains? target-x pos)))
                       (map (fn [[_ _ iter]] iter))
                       (reduce min))]
    (->> (range (- y-min) 0 -1)
         (map (fn [init-dy]
                (->> [0 init-dy 0]
                     (iterate (fn [[pos speed iter]]
                                [(+ pos speed) (dec speed) (inc iter)]))
                     (take-while (fn [[pos _ _]]
                                   (>= pos y-min))))))
         (filter (fn [traj]
                   (->> traj
                        (filter (fn [[pos _ iter]]
                                  (and (contains? target-y pos)
                                       (>= iter min-steps))))
                        seq)))
         (map (fn [traj]
                (->> traj
                     (map (fn [[pos _ _]] pos))
                     (reduce max))))
         first)))
