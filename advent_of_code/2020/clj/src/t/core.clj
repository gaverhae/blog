(ns t.core
  (:require [clojure.string :as string]))

(defn parse5
  [s]
  (->> (string/split-lines s)
       (map #(map {\F \0, \B \1, \L \0, \R \1} %))
       (map #(-> [(apply str (take 7 %))
                  (apply str (drop 7 %))]))
       (map #(map (fn [s] (Long/parseLong s 2)) %))))

(comment
  (->> (slurp "data/day5")
       parse5
       (map (fn [[r c]] (+ (* r 8) c)))
       (apply max))
826

  (->> (slurp "data/day5")
       parse5
       (map (fn [[r c]] (+ (* r 8) c)))
       sort
       (partition 2 1)
       (filter (fn [[p n]] (= 2 (- n p)))))
((677 679))

  )
