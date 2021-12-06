(ns t.day6
  (:require [clojure.string :as string]))

(defn one-day-later
  [old-pop]
  (reduce-kv (fn [new-pop age n]
               (merge-with + new-pop
                           (if (zero? age)
                             {8 n 6 n}
                             {(dec age) n})))
             {}
             old-pop))

(defn n-days-later
  [init-pop n-days]
  (-> (iterate one-day-later init-pop)
      (nth n-days)
      vals
      (->> (reduce +))))

(defn parse
  [lines]
  (-> lines
      first
      (string/split #",")
      (->> (map #(Long/parseLong %)))
      frequencies))

(defn part1
  [input]
  (n-days-later input 80))

(defn part2
  [input]
  (n-days-later input 256))
