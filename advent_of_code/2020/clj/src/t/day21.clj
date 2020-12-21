(ns t.day21
  (:require [clojure.set :as set]
            [instaparse.core :as insta]))

(defn parse
  [lines]
  (let [parse (insta/parser "<S> = ingredients allergens
                             ingredients = word+
                             allergens = <'(contains '> word (<','> word)* <')'>
                             <word> = w #'\\w+' w
                             <w> = <#'\\s*'>")]
    (->> lines
         (map (fn [line]
                (let [[ingredients allergens] (map rest (parse line))]
                  [(set ingredients) (set allergens)])))
         set)))

(defn part1
  [input]
  (let [ingredients-with-allergen (->> input
                                       (reduce (fn [acc [ings als]]
                                                 (reduce (fn [acc al]
                                                           (if (acc al)
                                                             (update acc al set/intersection ings)
                                                             (assoc acc al ings)))
                                                         acc
                                                         als))
                                               {})
                                       vals
                                       (reduce set/union))]
    (->> input
         (map first)
         (reduce (fn [acc el]
                   (+ acc (count (set/difference el ingredients-with-allergen))))
                 0))))

(defn part2
  [input])
