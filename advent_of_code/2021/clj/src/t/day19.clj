(ns t.day19)

(defn parse
  [lines]
  (let [scanner-line? (fn [l] (re-matches #"--- scanner \d+ ---" l))]
    (->> lines
         (partition-by scanner-line?)
         (remove (fn [g] (scanner-line? (first g))))
         (map #(remove #{""} %))
         (mapv #(mapv (fn [l]
                        (let [[_ x y z] (re-matches #"(-?\d+),(-?\d+),(-?\d+)" l)]
                          [(Long/parseLong x)
                           (Long/parseLong y)
                           (Long/parseLong z)]))
                      %)))))

(defn part1
  [input])

(defn part2
  [input])
