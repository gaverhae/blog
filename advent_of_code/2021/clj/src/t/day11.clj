(ns t.day11)

(defn parse
  [lines]
  (->> lines
       (map-indexed
         (fn [y line]
           (map-indexed
             (fn [x v]
               [[y x] (Long/parseLong (str v))])
             line)))
       (apply concat)
       (into {})))

(defn neighs
  [[y x]]
  (for [dy [-1 0 1]
        dx [-1 0 1]]
    [(+ y dy) (+ x dx)]))

(defn step
  [[state flashes]]
  (loop [state (->> state
                    (map (fn [[k v]] [k (inc v)]))
                    (into {}))
         flashes flashes]
    (if (->> state vals (every? #(< % 10)))
      [state flashes]
      (let [to-flash (->> state (filter (fn [[_ v]] (>= v 10))) (map key))
            to-increase (->> to-flash (mapcat neighs) (filter #(contains? state %)))]
        (recur (as-> state state
                    (reduce (fn [state pos]
                              (assoc state pos 0))
                            state
                            to-flash)
                    (reduce (fn [state pos]
                              (update state pos
                                      #(if (zero? %) % (inc %))))
                            state
                            to-increase))
               (+ flashes (count to-flash)))))))

(defn part1
  [input]
  (->> [input 0]
       (iterate step)
       (drop 100)
       first
       second))

(defn part2
  [input]
  (->> [input 0]
       (iterate step)
       (map-indexed (fn [idx [state _flashes]]
                      [idx (->> state vals (every? zero?))]))
       (filter second)
       ffirst))
