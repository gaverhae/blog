(ns t.day20)

(defn parse
  [lines]
  (->> lines
       (partition-by #{""})
       (remove #{[""]})
       (map (fn [tile]
              (let [[_ id] (re-matches #"Tile (\d+):" (first tile))
                    to-nums (fn [chars] (map {\# 1, \. 0} chars))]
                {:id (Long/parseLong id)
                 :borders [(to-nums (second tile))
                           (to-nums (rest (map last tile)))
                           (to-nums (last tile))
                           (to-nums (rest (map first tile)))]})))))

(defn part1
  [input]
  (let [rotate (fn [[top right bottom left]]
                 [(reverse left) top (reverse right) bottom])
        once (->> input
                 (mapcat :borders)
                 (mapcat (fn [x] [x (reverse x)]))
                 frequencies
                 (filter (fn [[val count]] (= count 1)))
                 (map first)
                 set)]
    (->> input
         (filter (fn [{:keys [borders]}]
                   (->> borders
                        (filter once)
                        count
                        (= 2))))
         (map :id)
         (reduce *))))

(defn part2
  [input])
