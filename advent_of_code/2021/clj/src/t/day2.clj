(ns t.day2
  (:require [clojure.string :as string]))

(defn parse
  [lines]
  (->> lines
       (map #(string/split % #" "))
       (map (fn [[w n]]
              (case w
                "forward" [(Long/parseLong n) 0]
                "down" [0 (Long/parseLong n)]
                "up" [0 (- (Long/parseLong n))])))))

(defn part1
  [input]
  (->> input
       (reduce (fn [[x y] [dx dy]]
                 [(+ x dx) (+ y dy)]))
       (apply *)))

(defn part2
  [input]
  (->> input
       (reduce (fn [[x y a] [dx dy]]
                 (if (zero? dy)
                   [(+ x dx) (+ y (* a dx)) a]
                   [(+ x dx) y (+ a dy)]))
               [0 0 0])
       (take 2)
       (apply *)))
