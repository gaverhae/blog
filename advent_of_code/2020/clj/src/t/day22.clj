(ns t.day22)

(defn parse
  [lines]
  (->> lines
       (partition-by  #{""})
       (remove #{[""]})
       (map (fn [d]
              (->> d rest (map #(Long/parseLong %)))))))

(defn part1
  [[init1 init2]]
  (loop [p1 init1
         p2 init2]
    (let [win (fn [deck] (->> deck
                              reverse
                              (map-indexed vector)
                              (reduce (fn [acc [idx c]]
                                        (+ acc (* (inc idx) c)))
                                      0)))]
    (cond (empty? p1) (win p2)
          (empty? p2) (win p1)
          (> (first p1) (first p2)) (recur (concat (rest p1) [(first p1) (first p2)])
                                           (rest p2))
          (< (first p1) (first p2)) (recur (rest p1)
                                           (concat (rest p2) [(first p2) (first p1)]))))))

(defn part2
  [input])
