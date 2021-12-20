(ns t.day20)

(defn parse
  [lines]
  {:alg (->> lines first (mapv {\. 0 \# 1}))
   :img (->> lines (drop 2) (mapv #(mapv {\. 0 \# 1} %)))})

(defn part1
  [input])

(defn part2
  [input])
