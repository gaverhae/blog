(ns t.day2
  (:require [clojure.string :as string]))

(defn parse
  [lines]
  (->> lines
       (map #(string/split % #" |: |-"))
       (map (fn [[min max c pw]]
              [(Long/parseLong min)
               (Long/parseLong max)
               (first c)
               pw]))))

(defn part1
  [input]
  (->> input
       (filter (fn [[min max c pw]]
                 (<= min
                     (count (filter #{c} pw))
                     max)))
       count))

(defn part2
  [input]
  (->> input
       (filter (fn [[min max ^char c ^String pw]]
                 (let [a (= c (.charAt pw (dec min)))
                       b (= c (.charAt pw (dec max)))]
                   (if a (not b) b))))
       count))
