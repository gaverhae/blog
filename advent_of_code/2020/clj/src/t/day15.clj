(ns t.day15
  (:require [clojure.string :as string]))

(defn parse
  [[line]]
  (map #(Long/parseLong %) (string/split line #",")))

(defn brute-force
  [stop input]
  (let [m (into {} (map-indexed (fn [idx n] [n (inc idx)]) (butlast input)))]
    (loop [n (count input)
           cur (last input)
           prevs m]
      (cond (= n stop) cur
            (prevs cur) (recur (inc n) (- n (prevs cur)) (assoc prevs cur n))
            :else (recur (inc n) 0 (assoc prevs cur n))))))


(defn part1
  [input]
  (brute-force 2020 input))

(defn part2
  [input]
  (brute-force 30000000 input))
