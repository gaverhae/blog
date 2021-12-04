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
                (remove #{'("")})
                (mapv (fn [b] (->> b
                                   (map #(string/split % #" "))
                                   (map (fn [l]
                                          (->> l
                                               (remove empty?)
                                               (mapv #(Long/parseLong %)))))
                                   (#(concat % (transpose %)))
                                   (map set)))))})

(defn winners
  [boards nums]
  (let [winner? (fn [b] (some #{#{}} b))]
    (->> (reductions
           (fn [[_ boards] n]
             (let [boards (->> boards
                               (map (fn [b] (map #(disj % n) b))))]
               [(->> boards
                     (filter winner?)
                     (map (fn [b] [b n])))
                (->> boards
                     (remove winner?))]))
           [[] boards]
           nums)
         (mapcat first)
         (map (fn [[board n]]
                (->> board
                     (reduce set/union)
                     (reduce +)
                     (* n)))))))

(defn part1
  [{:keys [boards numbers]}]
  (->> (winners boards numbers)
       first))

(defn part2
  [{:keys [boards numbers]}]
  (->> (winners boards numbers)
       last))
