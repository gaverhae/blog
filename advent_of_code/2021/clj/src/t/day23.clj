(ns t.day23
  (:require [t.util :refer [transpose]]))

(defn parse
  [lines]
  (->> lines
       (drop 2)
       (take 2)
       (map-indexed (fn [y line]
                      (map-indexed (fn [x c]
                                     (when-let [c ({\A 1 \B 10 \C 100 \D 1000} c)]
                                       [[(- x 1) (inc y)] c]))
                                   line)))
       (apply concat)
       (remove nil?)
       (into {})))

;(defn possible-moves
;  [{:keys [hallway halls]}]
;  (->> (concat (->> hallway
;                    (map-indexed (fn [idx c] (when (pos? c) [idx c])))
;                    (remove nil?))
;               (->> halls
;                    (map (fn [hall] (->> (map-indexed (fn [idx c] (when (pos? c) [idx c])) hall)
;                                         (remove nil?)
;                                         first)))
;                    (remove nil?)))
;       (map (fn [

(defn part1
  [input]
  #_(possible-moves input))

(defn part2
  [input])
