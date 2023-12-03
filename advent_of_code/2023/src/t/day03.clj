(ns t.day03
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [t.lib :as lib :refer [->long]]))

(defn all-indices
  [s pat]
  (loop [ps []
         idx 0]
    (if-let [i (s/index-of s pat idx)]
      (recur (conj ps i) (long (inc i)))
      ps)))

(defn parse
  [lines]
  {:numbers (->> lines
                 (map-indexed (fn [y line]
                                (let [nums (re-seq #"[0-9]+" line)]
                                  (->> nums
                                       (map (fn [n]
                                              (->> (all-indices line n)
                                                   (map (fn [x]
                                                          [n [y x]])))))
                                       (apply concat)))))
                 (apply concat))
   :symbols (->> lines
                 (map-indexed (fn [y line]
                                (let [syms (re-seq #"[^0-9.]" line)]
                                  (->> syms
                                       (map (fn [s]
                                              (->> (all-indices line s)
                                                   (map (fn [x]
                                                          [s [y x]])))))
                                       (apply concat)))))
                 (apply concat))})

(defn part1
  [{:keys [symbols numbers]}]
  (let [adj (->> symbols
                 (mapcat (fn [[_ [y x]]]
                           [[(dec y) (dec x)] [(dec y) x] [(dec y) (inc x)]
                            [     y  (dec x)] [     y  x] [     y  (inc x)]
                            [(inc y) (dec x)] [(inc y) x] [(inc y) (inc x)]]))
                 set)]
    (->> numbers
         (filter (fn [[n [y x0]]]
                   (let [ps (set (for [x (range x0 (+ x0 (count n)))] [y x]))]
                     (seq (set/intersection ps adj)))))
         (map (comp ->long first))
         (reduce + 0))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 4361
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
