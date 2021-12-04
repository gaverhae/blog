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
  (let [winner? #(some empty? %)
        draw (fn rec [boards nums]
               (when (seq nums)
                 (let [boards (->> boards
                                   (map (fn [b] (map #(disj % (first nums)) b))))]
                   (concat (->> boards
                                (filter winner?)
                                (map (fn [board]
                                       (->> board
                                            (reduce set/union)
                                            (reduce +)
                                            (* (first nums))))))
                           (lazy-seq (rec (remove winner? boards)
                                          (rest nums)))))))]
    (draw boards nums)))

(defn part1
  [{:keys [boards numbers]}]
  (->> (winners boards numbers)
       first))

(defn part2
  [{:keys [boards numbers]}]
  (->> (winners boards numbers)
       last))
