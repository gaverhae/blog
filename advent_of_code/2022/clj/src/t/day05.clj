(ns t.day05
  (:require [t.lib :as lib :refer [l]]))

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
                 (map (fn [c]
                        (let [[_ n from to] (re-matches #"move (\d+) from (\d+) to (\d+)" c)]
                          {:n (l n) :from (dec (l from)), :to (dec (l to))}))))}))

(defn solve
  [crates moves]
  (->> (reduce (fn [crates {:keys [n from to]}]
                 (-> crates
                     (update from #(drop n %))
                     (update to #(concat (take n (crates from)) %))))
               crates
               moves)
       (map first)
       (apply str)))

(defn part1
  [{:keys [crates moves]}]
  (solve crates (->> moves
                     (mapcat (fn [m] (repeat (:n m) (assoc m :n 1)))))))

(defn part2
  [{:keys [crates moves]}]
  (solve crates moves))

(lib/check
  parse
  part1 "CMZ" "SHMSDGZVC"
  part2 "MCD" "VRZGHDFBQ")
