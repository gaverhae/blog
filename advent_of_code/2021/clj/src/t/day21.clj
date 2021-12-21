(ns t.day21)

(defn parse
  [lines]
  (->> lines
       (map (fn [l] (let [[_ p s] (re-matches #"Player (\d) starting position: (\d+)" l)]
                      [(dec (Long/parseLong p)) (Long/parseLong s)])))
       (into {})))

(defn part1
  [input]
  (loop [positions input
         playing 0
         die 1
         score [0 0]
         n 0]
    (if (->> score (some (fn [x] (>= x 1000))))
      (->> score (filter #(< % 1000)) first (* n))
      (let [old-pos (get positions playing)
            die-roll (-> (+ die die 2)
                         (* 3)
                         (/ 2))
            new-pos (mod (+ old-pos die-roll) 10)]
        (recur (assoc positions playing new-pos)
               (- 1 playing)
               (long (mod (+ 3 die) 100))
               (update score playing (fn [s] (+ s (if (zero? new-pos)
                                                    10 new-pos))))
               (+ n 3))))))

(def dirac-rolls
  (->> (for [r1 [1 2 3]
             r2 [1 2 3]
             r3 [1 2 3]]
         (+ r1 r2 r3))
       frequencies))

(defn play-one-turn
  [[_won? universes positions playing score]]
  (let [old-pos (get positions playing)]
    (->> dirac-rolls
         (map (fn [[result num-univs]]
                (let [new-pos (mod (+ old-pos result) 10)
                      univs (* num-univs universes)
                      new-score (+ (get score playing)
                                   (if (zero? new-pos)
                                     10 new-pos))]
                  [(when (>= new-score 21) playing)
                   univs
                   (assoc positions playing new-pos)
                   (- 1 playing)
                   (assoc score playing new-score)]))))))

(defn part2
  [input]
  (loop [stack-of-realities (list [nil 1 input 0 [0 0]])
         win-p1 0
         win-p2 0]
    (if (empty? stack-of-realities)
      (max win-p1 win-p2)
      (let [r (first stack-of-realities)
            {player-1-won 0
             player-2-won 1
             still-going nil} (group-by first (play-one-turn r))]
        (recur (concat still-going (rest stack-of-realities))
               (long (reduce + win-p1 (->> player-1-won (map second))))
               (long (reduce + win-p2 (->> player-2-won (map second)))))))))
