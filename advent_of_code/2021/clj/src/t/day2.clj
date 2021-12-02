(ns t.day2
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]))

(defn parse
  [lines]
  (->> lines
       (map #(string/split % #" "))
       (map (fn [[w n]]
              [(keyword w) (Long/parseLong n)]))))

(defn part1
  [input]
  (->> input
       (reduce (fn [[f d] el]
                 (match el
                   [:forward x] [(+ f x) d]
                   [:up x] [f (- d x)]
                   [:down x] [f (+ d x)]))
               [0 0])
       (apply *)))

(defn part2
  [input]
  (->> input
       (reduce (fn [[f d a] el]
                 (match el
                   [:forward x] [(+ f x) (+ d (* a x)) a]
                   [:up x] [f d (- a x)]
                   [:down x] [f d (+ a x)]))
               [0 0 0])
       (take 2)
       (apply *)))
