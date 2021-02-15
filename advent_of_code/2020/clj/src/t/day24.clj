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
                           [0 0 0]))))
       (reduce (fn [acc el]
                 (update acc el (fnil not false)))
               {})
       (filter (fn [[pos black?]] black?))
       (map first)
       set))

(defn part1
  [input]
  (count input))

(defn part2
  [input]
  (let [neighbours (fn [[x y z]]
                     (map (fn [[dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)])
                          [[-1 0 1] [0 -1 1] [-1 1 0] [1 0 -1] [0 1 -1] [1 -1 0]]))]
    (->> (range 100)
         (reduce (fn [prev _]
                   (->> (mapcat neighbours prev)
                        (reduce (fn [acc el]
                                  (update acc el (fnil inc 0)))
                                {})
                        (keep (fn [[pos num-black-neighbours]]
                                ;; if zero black neighbours, does not appear
                                (when (or (and (or (== 1 num-black-neighbours)
                                                   (== 2 num-black-neighbours))
                                               (contains? prev pos))
                                          (and (== 2 num-black-neighbours)
                                               (not (contains? prev pos))))
                                  pos)))
                        set))
                 input)
         count)))
