(ns t.day02
  (:require [t.lib :as lib]))

(def rules
  [[1 1 3] [1 2 6] [1 3 0]
   [2 1 0] [2 2 3] [2 3 6]
   [3 1 6] [3 2 0] [3 3 3]])

(def parse identity)

(defn solve
  [parse-second score-fn input]
  (let [score (->> rules
                   (map score-fn)
                   (into {}))]
        (->> input
             (map (fn [s] [({\A 1, \B 2, \C 3} (get s 0))
                           (parse-second (get s 2))]))
             (map score)
             (reduce + 0))))

(defn part1
  [input]
  (solve {\X 1, \Y 2, \Z 3}
         (fn [[a b c]] [[a b] (+ b c)])
         input))

(defn part2
  [input]
  (solve {\X 0, \Y 3, \Z 6}
         (fn [[a b c]] [[a c] (+ b c)])
         input))

(lib/check
  [part1 sample] 15
  [part1 puzzle] 13268
  [part2 sample] 12
  [part2 puzzle] 15508)
