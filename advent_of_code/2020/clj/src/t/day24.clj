(ns t.day24)

(defn parse
  [lines]
  (->> lines
       (map (fn [l]
              (->> (re-seq #"e|se|sw|w|nw|ne" l)
                   (map {"se" [-1 0 1]
                         "sw" [0 -1 1]
                         "e" [-1 1 0]
                         "nw" [1 0 -1]
                         "ne" [0 1 -1]
                         "w" [1 -1 0]})
                   (reduce (fn [acc el]
                             [(+ (acc 0) (el 0))
                              (+ (acc 1) (el 1))
                              (+ (acc 2) (el 2))])
                           [0 0 0]))))))

(defn part1
  [input]
  (->> input
       (reduce (fn [acc el]
                 (update acc el (fnil not false)))
               {})
       (filter (fn [[pos black?]] black?))
       count))

(defn part2
  [input])
