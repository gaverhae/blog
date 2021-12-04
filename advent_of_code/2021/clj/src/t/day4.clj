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

(defn part1
  [input]
  (let [rows-and-lines (->> (:boards input)
                            (map (fn [lines]
                                   (->> (concat lines
                                                (transpose lines))
                                        (map set)))))
        compute-score (fn [board drawn]
                        (-> (reduce set/union board)
                            (set/difference (set drawn))
                            (->> (reduce +))
                            (* (last drawn))))
        check-winner (fn [drawn]
                       (if-let [winner (->> rows-and-lines
                                            (filter (fn [board]
                                                      (->> board
                                                           (filter (fn [rol] (= rol (set/intersection rol (set drawn)))))
                                                           count
                                                           (= 1))))
                                            first)]
                         (compute-score winner drawn)
                         nil))]
    (->> (range)
         (some (fn [i] (check-winner (take i (:numbers input))))))))

(defn part2
  [input]
  (let [rows-and-lines (->> (:boards input)
                            (map (fn [lines]
                                   (->> (concat lines
                                                (transpose lines))
                                        (map set)))))
        compute-score (fn [board drawn]
                        (-> (reduce set/union board)
                            (set/difference (set drawn))
                            (->> (reduce +))
                            (* (last drawn))))
        pick-winner (fn [drawn boards]
                       (->> boards
                            (filter (fn [board]
                                      (->> board
                                           (filter (fn [rol] (= rol (set/intersection rol (set drawn)))))
                                           count
                                           (<= 1))))
                            first))]
    (loop [boards (set rows-and-lines)
           idx 0]
      (prn [idx (count (:numbers input))])
      (if (and (= 1 (count boards))
               (pick-winner (take (inc idx) (:numbers input)) boards))
        (compute-score (first boards) (take (inc idx) (:numbers input)))
        (if-let [winner (pick-winner (take idx (:numbers input)) boards)]
          (recur (set/difference boards #{winner}) idx)
          (recur boards (inc idx)))))))
