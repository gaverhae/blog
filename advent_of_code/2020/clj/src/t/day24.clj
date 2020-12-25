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
    (loop [n 0
           is-black? input]
      (if (= n 100)
        (count is-black?)
        (recur (inc n)
               (->> is-black?
                    (mapcat (fn [t] (cons t (neighbours t))))
                    (filter (fn [pos]
                              (let [bn (->> (neighbours pos)
                                            (filter is-black?)
                                            count)
                                    black? (is-black? pos)]
                                (cond (and black? (zero? bn)) false
                                      (and black? (> bn 2)) false
                                      (and (not black?) (= 2 bn)) true
                                      :else black?))))
                    (reduce conj #{})))))))
