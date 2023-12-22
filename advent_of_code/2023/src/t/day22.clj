(ns t.day22
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
  (->> lines
       (map-indexed (fn [idx line]
                      [idx (-> line
                               (s/split #"~")
                               (->> (map #(s/split % #","))
                                    (mapv (fn [s] (mapv parse-long s)))))]))
       (into {})))

(defn brick->cells
  [[[x0 y0 z0] [x1 y1 z1]]]
  (cond (= [x0 y0 z0] [x1 y1 z1]) [[x0 y0 z0]]
        (= [x0 y0] [x1 y1]) (->> (range z0 (inc z1))
                                 (map (fn [z] [x0 y0 z])))
        (= [x0 z0] [x1 z1]) (->> (range y0 (inc y1))
                                 (map (fn [y] [x0 y z0])))
        (= [y0 z0] [y1 z1]) (->> (range x0 (inc x1))
                                 (map (fn [x] [x y0 z0])))))

(defn fall
  [bricks]
  (loop [falling (->> bricks
                      (sort-by (fn [[_ [[x0 y0 z0] [x1 y1 z1]]]] (min z0 z1))))
         fallen {}
         occupied-cells #{}]
    (if (empty? falling)
      fallen
      (let [[[idx [[x0 y0 z0] [x1 y1 z1] :as brick]] & falling] falling
            cells (brick->cells brick)]
        (if (or (= 1 z0) (= 1 z1)
                (->> cells (map (fn [[x y z]] [x y (dec z)])) (some occupied-cells)))
          (recur falling (assoc fallen idx brick) (reduce conj occupied-cells cells))
          (recur (cons [idx [[x0 y0 (dec z0)] [x1 y1 (dec z1)]]] falling) fallen occupied-cells))))))

(defn part1
  [input]
  (let [fallen-bricks (fall input)]
    (->> fallen-bricks
         (filter (fn [b]
                   (let [other-bricks (->> fallen-bricks (remove #{b}))]
                     (= (set other-bricks) (set (fall other-bricks))))))
         count)))

(defn part2
  [input]
  (let [fallen-bricks (fall input)]
    (->> fallen-bricks
         (map (fn [b]
                (let [other-bricks (->> fallen-bricks (remove #{b}))]
                  (count (set/difference (set other-bricks) (set (fall other-bricks)))))))
         (reduce + 0))))

(lib/check
  #_#_[part1 sample] 5
  #_#_[part1 puzzle] 468
  #_#_[part2 sample] 7
  #_#_[part2 puzzle] 75358)
