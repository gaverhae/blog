(ns t.day4
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [t.util :refer [transpose]]))

(defn parse
  [lines]
  {:numbers (-> lines
                first
                (string/split #",")
                (->> (mapv #(Long/parseLong %))))
   :boards (->> lines
                rest
                (partition-by empty?)
                (remove #{[""]})
                (mapv (fn [b] (->> b
                                   (map #(string/split % #" "))
                                   (map (fn [l]
                                          (->> l
                                               (remove empty?)
                                               (mapv #(Long/parseLong %)))))
                                   (#(concat % (transpose %)))
                                   (map set)))))})

(defn winner?
  [board]
  (boolean (some empty? board)))

(defn score
  [n]
  (fn [board]
    (->> board
         (reduce set/union)
         (reduce +)
         (* n))))

(defn mark
  [n]
  (fn [board]
    (map #(disj % n) board)))

(defn winners
  [boards nums]
  (when-let [[n & nums] (seq nums)]
    (let [{won true, remaining false}
          (->> boards
               (map (mark n))
               (group-by winner?))]
      (concat (map (score n) won)
              (lazy-seq (winners remaining nums))))))

(defn part1
  [{:keys [boards numbers]}]
  (first (winners boards numbers)))

(defn part2
  [{:keys [boards numbers]}]
  (last (winners boards numbers)))
