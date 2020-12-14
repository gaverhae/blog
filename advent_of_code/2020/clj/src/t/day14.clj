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
                            (map-indexed (fn [idx c]
                                           [idx ({\0 0, \1 1 \X :float} c)]))
                            (into {}))]
                (let [[_ addr value] (re-matches #"mem\[(\d+)\] = (\d+)" line)]
                  [:mem (Long/parseLong addr) (Long/parseLong value)]))))))

(defn set-1
  [n pos]
  (bit-or n (bit-shift-left 1 pos)))

(defn set-0
  [n pos]
  (bit-and-not n (bit-shift-left 1 pos)))

(defn part1
  [input]
  (->> input
       (reduce (fn [[mask mem] [cmd arg1 arg2]]
                 (case cmd
                   :mask [arg1 mem]
                   :mem [mask (assoc mem
                                     arg1
                                     (reduce (fn [n [pos v]]
                                               (case v
                                                 0 (set-0 n pos)
                                                 1 (set-1 n pos)
                                                 :float n))
                                             arg2
                                             mask))]))
               [{} {}])
       second
       vals
       (reduce +)))

(defn part2
  [input]
  (->> input
       (reduce (fn [[mask mem] [cmd arg1 arg2]]
                 (case cmd
                   :mask [arg1 mem]
                   :mem (let [addrs (reduce (fn [addrs [pos v]]
                                               (case v
                                                 0 addrs
                                                 1 (map #(set-1 % pos) addrs)
                                                 :float (mapcat #(-> [(set-0 % pos) (set-1 % pos)]) addrs)))
                                             [arg1]
                                             mask)]
                          [mask (reduce (fn [mem addr]
                                          (assoc mem addr arg2))
                                        mem
                                        addrs)])))
               [{} {}])
       second
       vals
       (reduce +)))
