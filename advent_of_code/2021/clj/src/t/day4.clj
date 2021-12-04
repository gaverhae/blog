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
                                               (mapv #(Long/parseLong %)))))))))})

(defn rows-and-lines
  [boards]
  (->> boards
       (map (fn [lines]
              (->> (concat lines (transpose lines))
                   (map set))))))

(defn compute-score
  [drawn]
  (fn [ral]
    (-> (reduce set/union ral)
        (set/difference (set drawn))
        (->> (reduce +))
        (* (last drawn)))))

(defn winner?
  [drawn]
  (let [drawn (set drawn)]
    (fn [ral]
      (->> ral
           (filter #(set/subset? % drawn))
           count
           (<= 1)))))

(defn part1
  [input]
  (let [rals (rows-and-lines (:boards input))
        nums (:numbers input)]
    (->> (for [i (range (count nums))
               :let [drawn (take (inc i) nums)
                     winners (filter (winner? drawn) rals)]
               :when (not-empty winners)]
           (map (compute-score drawn) winners))
         ffirst)))

(defn part2
  [input]
  (let [rals (rows-and-lines (:boards input))]
    (loop [boards (set rals)
           idx 0]
      (let [drawn (take idx (:numbers input))]
        (if (and (= 1 (count boards))
                 ((winner? drawn) (first boards)))
          ((compute-score drawn) (first boards))
          (recur (remove (winner? drawn) boards)
                 (inc idx)))))))
