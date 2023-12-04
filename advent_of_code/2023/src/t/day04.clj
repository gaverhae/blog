(ns t.day04
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [[[id] winning have] (->> (s/split line #"\||:")
                                          (map (fn [s] (map parse-long (re-seq #"\d+" s)))))]
                [id (count (set/intersection (set winning) (set have)))])))
       (into {})))

(defn part1
  [input]
  (->> input
       (keep (fn [[id c]] (when (pos? c) (long (math/pow 2 (dec c))))))
       (reduce + 0)))

(defn part2
  [input]
  (loop [haves (->> input (map (fn [[k v]] [k 1])) (into {}))
         cards (->> input keys sort)]
    (if (empty? cards)
      (->> haves vals (reduce + 00))
      (let [id (first cards)
            num-copies (haves id)
            matching (input id)]
        (recur (reduce (fn [acc el]
                         (update acc el + num-copies))
                       haves
                       (range (inc id) (+ (inc id) matching)))
               (rest cards))))))

(lib/check
  [part1 sample] 13
  [part1 puzzle] 25183
  [part2 sample] 30
  [part2 puzzle] 5667240)
