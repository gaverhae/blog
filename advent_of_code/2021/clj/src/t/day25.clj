(ns t.day25)

(defn parse
  [lines]
  (->> lines
       (map-indexed (fn [y line]
                      (->> line
                           (map-indexed
                             (fn [x c]
                               (case c
                                 \v [[y x] :down]
                                 \> [[y x] :right]
                                 \. [])))
                           (remove #{[]}))))
       (apply concat)
       (into {})))

(defn part1
  [input])

(defn part2
  [input])
