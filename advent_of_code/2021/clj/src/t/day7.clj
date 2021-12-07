(ns t.day7
  (:require [clojure.string :as string]))

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
                    (reduce + 0)))
        low (apply min input)
        high (apply max input)]
    (->> (range low (inc high))
         (map (fn [n] [(cost n) n]))
         sort
         first
         first)))


(defn part2
  [input]
  (let [cost (fn [target]
               (->> input
                    (map (fn [p] (- (max p target) (min p target))))
                    (map (fn [c] (reduce + (range (inc c)))))
                    (reduce + 0)))
        low (apply min input)
        high (apply max input)]
    (->> (range low (inc high))
         (map (fn [n] [(cost n) n]))
         sort
         first
         first)))
