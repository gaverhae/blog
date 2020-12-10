(ns t.day10)

(defn parse
  [lines]
  (map #(Long/parseLong %) lines))

(defn part1
  [input]
  (->> (cons 0 input)
       sort
       (partition 2 1)
       (map (fn [[a  b]] (- b a)))
       (cons 3)
       frequencies
       vals
       (apply *)))

(defn part2
  [input])
