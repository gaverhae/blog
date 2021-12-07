(ns t.day7
  (:require [clojure.string :as string]))

(defn search
  [space cost]
  (loop [low (apply min space)
         high (apply max space)]
   (let [mid (quot (+ low high) 2)
         [a b c] (mapv cost [(dec mid) mid (inc mid)])]
     (cond (and (< b a) (< b c)) b
           (<= a b c) (recur low mid)
           (>= a b c) (recur mid high)))))

(defn parse
  [lines]
  (-> lines
      first
      (string/split #",")
      (->> (map #(Long/parseLong %)))))

(defn part1
  [input]
  (let [cost (fn [target]
               (->> input
                    (map (fn [p] (- (max p target) (min p target))))
                    (reduce + 0)))]
    (search input cost)))

(defn part2
  [input]
  (let [cost (fn [target]
               (->> input
                    (map (fn [p] (- (max p target) (min p target))))
                    (map (fn [c] (reduce + (range (inc c)))))
                    (reduce + 0)))]
    (search input cost)))
