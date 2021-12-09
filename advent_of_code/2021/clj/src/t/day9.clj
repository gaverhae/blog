(ns t.day9
  (:require [t.util :as util]))

(defn parse
  [lines]
  (->> lines
       (mapv (fn [line]
              (->> line
                   (mapv (fn [c] (Long/parseLong (str c)))))))))

(defn neighbours
  [[y x]]
  [[(dec y) x] [(inc y) x] [y (inc x)] [y (dec x)]])

(defn get-height
  [table]
  (fn [[y x]]
    (get-in table [y x] 10)))

(defn low-points
  [table]
  (for [x (range (count (first table)))
        y (range (count table))
        :let [pos [y x]
              v ((get-height table) pos)]
        :when (->> (neighbours pos)
                   (map (get-height table))
                   (every? #(< v %)))]
    pos))

(defn part1
  [input]
  (->> input
       low-points
       (map (get-height input))
       (map inc)
       (reduce + 0)))

(defn basin
  [table]
  (fn [low-point]
    (loop [basin (set [low-point])]
      (let [new-basin (->> basin
                           (mapcat (fn [[y x]]
                                     [[y x] [(dec y) x] [(inc y) x] [y (inc x)] [y (dec x)]]))
                           (remove (fn [[y x]]
                                     (= 9 (get-in table [y x] 9))))
                           set)]
        (if (= new-basin basin)
          basin
          (recur new-basin))))))

(defn part2
  [input]
  (->> input
       low-points
       (map (basin input))
       (map count)
       sort
       reverse
       (take 3)
       (reduce * 1)))
