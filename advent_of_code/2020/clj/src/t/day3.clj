(ns t.day3)

(defn parse
  [lines]
  (->> lines
       (map #(map {\. 0 \# 1} %))
       (map cycle)))

(defn slope
  [[right down] geo]
  (loop [n 0
         pos geo]
    (if (empty? pos)
      n
      (recur (+ n (ffirst pos))
             (drop down (map #(drop right %) pos))))))

(def part1 (partial slope [3 1]))

(defn part2
  [terrain]
  (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
       (map #(slope % terrain))
       (reduce *)))

