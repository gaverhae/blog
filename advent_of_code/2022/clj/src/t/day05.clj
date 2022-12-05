(ns t.day05
  (:require [t.lib :as lib :refer [l]]
            [clojure.set :as set]
            [clojure.string :as string]))

(defn parse
  [lines]
  (let [[crates _ moves] (partition-by #{""} lines)]
    {:crates (->> (butlast crates)
                  (map (fn [line]
                         (->> line
                              (partition 4 4 (repeat \space))
                              (map (fn [s] (nth s 1))))))
                  lib/transpose
                  (map #(remove #{\space} %))
                  vec)
     :moves (->> moves
                 (mapcat (fn [c]
                           (let [[_ n from to] (re-matches #"move (\d+) from (\d+) to (\d+)" c)]
                             (repeat (l n) {:from (dec (l from)), :to (dec (l to))})))))}))

(defn part1
  [{:as i :keys [crates moves]}]
  (->> (reduce (fn [crates {:keys [from to]}]
                 (-> crates
                     (update from rest)
                     (update to #(cons (first (crates from)) %))))
               crates
               moves)
       (map first)
       (apply str)))

(defn part2
  [input]
  )

(lib/check
  parse
  part1 "CMZ" ""
  #_part2)
