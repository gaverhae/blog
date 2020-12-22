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
  [[init1 init2]]
  (let [win-round (fn [winner loser]
                    [(concat (rest winner) [(first winner) (first loser)])
                     (rest loser)])
        play-game (fn play-game [[p1 p2] mem]
                    (cond (contains? mem [p1 p2]) [0 p1]
                          (empty? p1) [1 p2]
                          (empty? p2) [0 p1]

                          (and (> (count p1) (first p1))
                               (> (count p2) (first p2)))
                          (let [sub1 (take (first p1) (rest p1))
                                sub2 (take (first p2) (rest p2))
                                [winner _] (play-game [sub1 sub2] #{})]
                            (case winner
                              0 (recur (win-round p1 p2) (conj mem [p1 p2]))
                              1 (recur (reverse (win-round p2 p1)) (conj mem [p1 p2]))))

                          (> (first p1) (first p2))
                          (recur (win-round p1 p2)
                                 (conj mem [p1 p2]))

                          (< (first p1) (first p2))
                          (recur (reverse (win-round p2 p1))
                                 (conj mem [p1 p2]))))
        [_ winner-deck] (play-game [init1 init2] #{})]
    (->> winner-deck
         reverse
         (map-indexed vector)
         (reduce (fn [acc [idx c]]
                   (+ acc (* (inc idx) c)))
                 0))))
