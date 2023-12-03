(ns t.day02
  (:require [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(def parser
  (insta/parser
    "<S> = <'Game '> num <': '> draws
    <draws> = draw (<', ' | '; '> draw)*
    draw = num <' '> color
    <num> = #'\\d+'
    <color> = 'red' | 'green' | 'blue'"))

(defn parse
  [lines]
  (->> (map parser lines)
       (map (fn [[id & draws]]
              (cons (->long id)
                    (->> ["red" "green" "blue"]
                         (map (fn [color]
                                (->> draws
                                     (filter (fn [[_ _ c]] (= c color)))
                                     (map (fn [[_ n _]] (->long n)))
                                     (reduce (fnil max 0)))))))))))

(defn part1
  [input]
  (->> input
       (filter (fn [[id red green blue]]
                 (and (<= red 12)
                      (<= green 13)
                      (<= blue 14))))
       (map first)
       (reduce + 0)))

(defn part2
  [input]
  (->> input
       (map (fn [[id red green blue]]
              (* blue green red)))
       (reduce + 0)))

(lib/check
  [part1 sample] 8
  [part1 puzzle] 2593
  [part2 sample] 2286
  [part2 puzzle] 54699)
