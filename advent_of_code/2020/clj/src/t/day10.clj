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

(def arrangements
  (memoize
    (fn [n]
      (->> (loop [i n
                  prev [[]]]
             (if (zero? i)
               prev
               (recur (dec i)
                      (mapcat #(-> [(cons 0 %) (cons 1 %)]) prev))))
           (remove (fn [bin]
                     (some->> bin
                              (partition-by #{0})
                              (filter (comp #{0} first))
                              not-empty
                              (map count)
                              (apply max)
                              (< 2))))
           count))))

(defn part2
  [input]
  (->> input
       (partition-by #{1})
       (filter (comp #{1} first))
       (map count)
       (map dec)
       (map arrangements)
       (reduce *)))
