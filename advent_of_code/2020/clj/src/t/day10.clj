(ns t.day10)

(defn parse
  [lines]
  (->> lines
       (map #(Long/parseLong %))
       sort
       ((fn [nums]
          (concat [0] nums [(+ 3 (last nums))])))
       (partition 2 1)
       (map (fn [[a  b]] (- b a)))))

(defn part1
  [input]
  (->> input
       frequencies
       vals
       (apply *)))

(defn part2
  [input]
  (->> input
       (partition-by #{1})
       (filter (comp #{1} first))
       (map count)
       (map dec)
       (map [1 2 4 7]) ;; #(-> (iterate (fn [[x y z]] [y z (+ x y z)])
                       ;;               [1 2 4])
                       ;;      (nth %)
                       ;;      first)
       (reduce *)))
