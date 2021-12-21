(ns t.day21)

(defn parse
  [lines]
  (->> lines
       (map (fn [l] (let [[_ p s] (re-matches #"Player (\d) starting position: (\d+)" l)]
                      [(Long/parseLong p) (Long/parseLong s)])))
       (into {})))

(defn part1
  [input])

(defn part2
  [input])
