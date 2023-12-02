(ns t.day01
  (:require [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  lines)

(defn solve
  [lines regex]
  (->> lines
       (map (fn [line]
              (->> (reductions (fn [acc _] (apply str (rest acc))) line line)
                   butlast
                   (keep (fn [s] (re-find regex s)))
                   (map #({"one" "1", "two" "2", "three" "3", "four" 4, "five" "5", "six" "6", "seven" "7", "eight" "8", "nine" "9"} % %))
                   ((juxt first last))
                   (apply str)
                   ->long)))
       (reduce + 0)))

(defn part1
  [input]
  (solve input #"[0-9]"))

(defn part2
  [input]
  (solve input #"[0-9]|one|two|three|four|five|six|seven|eight|nine"))

(lib/check
  [part1 sample] 142
  [part1 puzzle] 53921
  [part2 sample] 142
  [part2 puzzle] 54676)
