(ns t.day1)

(defn parse
  [lines]
  (vec (map #(Long/parseLong %) lines)))

(defn part1
  [input]
  (->> input
       (partition 2 1)
       (filter (fn [[a b]] (< a b)))
       count))

(defn part2
  [input]
  (->> input
       (partition 3 1)
       (map (fn [v] (reduce + 0 v)))
       part1))
