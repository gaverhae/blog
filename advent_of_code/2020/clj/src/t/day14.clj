(ns t.day14)

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (if (.startsWith ^String line "mask")
                [:mask (->> line
                            (re-matches #"mask = ([X10]+)")
                            second
                            reverse
                            (keep-indexed (fn [idx c]
                                            (when (#{\1 \0} c)
                                              [idx ({\0 0, \1 1} c)])))
                            (into {}))]
                (let [[_ addr value] (re-matches #"mem\[(\d+)\] = (\d+)" line)]
                  [:mem (Long/parseLong addr) (Long/parseLong value)]))))))

(defn apply-mask
  [mask n]
  (reduce (fn [n [pos v]]
            (case v
              0 (bit-and-not n (bit-shift-left 1 pos))
              1 (bit-or n (bit-shift-left 1 pos))))
          n
          mask))

(defn part1
  [input]
  (->> input
       (reduce (fn [[mask mem] [cmd arg1 arg2]]
                 (case cmd
                   :mask [arg1 mem]
                   :mem [mask (assoc mem arg1
                                     (apply-mask mask arg2))]))
               [{} {}])
       second
       vals
       (reduce +)))

(defn part2
  [input])
