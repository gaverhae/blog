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
       (map (fn [line]
              (-> line
                  (s/split #"~")
                  (->> (map #(s/split % #","))
                       (mapv (fn [s] (mapv parse-long s)))))))))

(defn brick->cells
  [[[x0 y0 z0] [x1 y1 z1]]]
  (cond (= [x0 y0 z0] [x1 y1 z1]) [[x0 y0 z0]]
        (= [x0 y0] [x1 y1]) (->> (range z0 (inc z1))
                                 (map (fn [z] [x0 y0 z])))))


(defn fall
  [bricks]
  (loop [falling (->> bricks
                      (sort-by (fn [[[x0 y0 z0] [x1 y1 z1]]] (min z0 z1)))
                      (into clojure.lang.PersistentQueue/EMPTY))
         fallen []
         occupied-cells #{}]
    (if (empty? falling)
      fallen
      (let [[[[x0 y0 z0] [x1 y1 z1] :as brick] falling] ((juxt peek pop) falling)
            cells (brick->cells brick)]
        (if (or (zero? z0) (zero? z1))
          (recur falling (conj fallen brick) (reduce conj occupied-cells cells))
          :todo)))))

(defn part1
  [input]
  [(->> input
       (every? (fn [[[x0 y0 z0] [x1 y1 z1]]] (and (<= x0 x1) (<= y0 y1) (<= z0 z1)))))
   (->> input
        (every? (fn [[[x0 y0 z0] [x1 y1 z1]]]
                  (or (= [x0 y0 z0] [x1 y1 z1])
                      (= [x0 y0] [x1 y1])
                      (= [x0 z0] [x1 z1])
                      (= [y0 z0] [y1 z1])))))]
  #_(fall input))


(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 0
  [part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
