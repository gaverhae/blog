(ns flapjack.core
  (:import [java.util UUID]))

(defn half-deck
  "Returns a shuffled half-deck (13 red cards, 13 black cards). Black cards
  count as positive, red cards count as negative."
  []
  (let [one-color (concat (range 1 11) ;; 1 to 10, black
                          [10 10 10])] ;; Jack, Queen and King
    (shuffle (concat one-color (map - one-color)))))

(defn scores
  "Given a hand, returns a vector of possible scores for it. Ones can count as
  eleven."
  [hand]
  (let [is-ace? (fn [c] (= 1 (* c c)))]
    (reduce (fn [scores card]
              (concat (map (fn [s] (+ s card)) scores)
                      (when (is-ace? card)
                        (map (fn [s] (+ s (* 11 card))) scores))))
            [0] hand)))

(defn points
  "Given a hand, returns the list of possible points for it."
  [hand]
  (map (fn [score] (if (<= 16 score 25)
                     (- 25 score)
                     10))
       (scores hand)))

(defn best-points
  "Given a hand, returns the best possible points for it."
  [hand]
  (reduce min (points hand)))

(defn busted?
  [hand]
  (every? #(> % 25) (scores hand)))

(defn unattended-round
  "Given a strategy and a deck, runs through a complete round unattended, and
  returns the final hand & points. A strategy is a function which, given a
  hand, returns true to stop drawing cards."
  [stop? deck]
  (loop [d (rest deck)
         h [(first deck)]]
    (cond (empty? d) [h (best-points h)]
          (busted? h) [h 10]
          (stop? h) [h (best-points h)]
          :else (recur (rest d) (conj h (first d))))))

(defn test-strategy
  [stop? n]
  (->> (repeatedly n half-deck)
       (map #(unattended-round stop? %))
       (map second)
       (reduce +)))

(defn strat-take-n
  "Strategy that always takes n cards."
  [n]
  (fn [h] (= n (count h))))

(defn test-strategy-oracle
  [stop? n]
  (->> (repeatedly n half-deck)
       (map #(unattended-round (stop? %) %))
       (map second)
       (reduce +)))

(defn slow-best-play
  [deck]
  (let [all-hands (rest (reductions conj [] deck))
        first-bust (or (some->> all-hands
                                (filter busted?)
                                first
                                count)
                       (inc (count all-hands)))]
    (->> all-hands
         (take (dec first-bust))
         (sort-by best-points)
         first
         count)))

(defn best-play
  "Given a deck, returns the best place to stop drawing and the corresponding
  score."
  [deck]
  (let [score-add (fn [scores card]
                    (concat (map (fn [s] (+ s card)) scores)
                            (when (= 1 (* card card))
                              (map (fn [s] (+ s (* 11 card))) scores))))
        points-from-scores (fn [scores]
                             (->> scores
                                  (map (fn [score] (if (<= 16 score 25)
                                                     (- 25 score)
                                                     10)))
                                  (reduce min)))
        busted? (fn [scores] (every? #(> % 25) scores))
        best-of (fn [[n1 s1] [n2 s2]]
                  (if (< s2 s1) [n2 s2] [n1 s1]))]
    (loop [s (score-add [0] (first deck))
           best-so-far [1 (points-from-scores s)]
           n 2
           d (rest deck)]
      (if (or (busted? s) (empty? d)) (first best-so-far)
        (let [new-s (score-add s (first d))]
          (recur new-s
                 (best-of best-so-far [n (points-from-scores new-s)])
                 (inc n)
                 (rest d)))))))


(comment
(with-out-str (time (dotimes [_ 10000]
        (slow-best-play (half-deck)))))
"\"Elapsed time: 12989.714633 msecs\"\n"
(with-out-str (time (dotimes [_ 10000]
        (best-play (half-deck)))))
"\"Elapsed time: 459.123759 msecs\"\n"

  (loop [d (half-deck)
         n 1000]
    (cond (zero? n)
          :ok

          (= (previous-best-play d) (best-play d))
          (recur (half-deck) (dec n))

          :else [d (previous-best-play d) (best-play d)
                 (->> d (reductions conj []) (map best-points))]))
:ok

  )

(defn monte-carlo-ish
  "Plays n random games where the deck starts with the current hand and keeps
  going if the average best play is in the future."
  [n]
  (let [f (frequencies (half-deck))]
    (fn [hand]
      (let [remaining-deck (->> (reduce (fn [acc el]
                                          (if (= 1 (acc el))
                                            (dissoc acc el)
                                            (update acc el dec)))
                                        f
                                        hand)
                                (mapcat (fn [[i n]] (repeat n i))))
            average-best-play (/ (->> (range 0 n)
                                      (map (fn [_]
                                             (best-play (concat hand (shuffle remaining-deck)))))
                                      (reduce +))
                                 n)]
        (> (count hand) average-best-play)))))

(defn one-card-probs
  "Always keeps playing if current points is 10. If current points are better,
  stops if probability of going bust is greater than bust-max, or if
  probability of improving the score is no greater than imp-min."
  [bust-max imp-min]
  (fn [hand]
      (let [p (best-points hand)]
        (and (not= 10 p)
             (let [possible-hands (->> hand
                                       (reduce (fn [acc el]
                                                 (if (= 1 (acc el))
                                                   (dissoc acc el)
                                                   (update acc el dec)))
                                               (frequencies (half-deck)))
                                       (mapcat (fn [[i n]] (repeat n i)))
                                       (map (fn [c] (conj hand c))))
                   p-bust (/ (count (filter busted? possible-hands))
                             (count possible-hands))
                   p-imp (/ (count (filter #(< (best-points %) p) possible-hands))
                            (count possible-hands))]
               (or (> p-bust bust-max)
                   (< p-imp imp-min)))))))



(comment

  (for [n (range 0 9)]
    [n (test-strategy (fn [_] (> (rand) (/ n 10))) 10000)])
([0 100000]
 [1 99635]
 [2 99215]
 [3 98725]
 [4 98075]
 [5 97702]
 [6 96987]
 [7 96190]
 [8 95109])

  (for [n (range 20)]
    [n (test-strategy (fn [h] (= n (count h))) 10000)])
([0 100000]
[1 100000]
[2 96591]
[3 94267]
[4 93423]
[5 93095]
[6 93293]
[7 92872]
[8 92224]
[9 92435]
[10 92719]
[11 92358]
[12 91904]
[13 91774]
[14 91541]
[15 91831]
[16 91518]
[17 91405]
[18 91547]
[19 91091])

(for [n (range 0 11)]
  [n (test-strategy (fn [h] (<= (best-points h) n)) 10000)])
([0 82690]
[1 72047]
[2 64096]
[3 59999]
[4 57934]
[5 57945]
[6 57195]
[7 60065]
[8 62860]
[9 67645]
[10 100000])

(test-strategy-oracle
  (fn [deck]
    (strat-take-n (best-play deck)))
  10000)
45761

(test-strategy (monte-carlo-ish 100) 100)
902
100
96

(->> (for [n (range 0 11)
           m (range 0 11)]
       [n m (test-strategy (one-card-probs (/ n 10) (/ m 10)) 1000)])
     (sort-by #(nth % 2)))
([9 2 5712] [6 2 5716] [3 2 5731] [8 2 5750] [7 2 5818] [5 2 5839] [6 1 5850] [4 2 5861] [2 1 5871] [10 2 5878] [3 1 5880] [2 2 5904] [10 1 5944] [9 1 6016] [8 1 6020] [7 1 6040] [4 1 6058] [5 1 6059] [0 1 6121] [1 2 6137] [5 3 6183] [6 3 6203] [0 2 6248] [1 1 6276] [3 3 6289] [2 3 6294] [10 3 6315] [9 3 6322] [8 3 6328] [4 3 6360] [7 3 6384] [1 3 6442] [9 6 6475] [4 8 6531] [0 3 6541] [3 8 6548] [2 4 6569] [6 4 6574] [1 5 6583] [8 9 6590] [2 5 6604] [6 5 6604] [6 9 6606] [8 6 6610] [3 4 6611] [10 4 6616] [8 4 6620] [7 5 6627] [8 5 6627] [5 10 6631] [5 4 6638] [7 6 6641] [4 10 6643] [5 9 6651] [8 8 6656] [6 7 6659] [9 10 6664] [5 8 6679] [9 5 6682] [9 9 6687] [3 7 6688] [6 10 6688] [6 6 6693] [9 4 6694] [4 9 6699] [5 5 6701] [10 5 6709] [7 4 6711] [3 10 6717] [2 9 6720] [8 10 6721] [5 7 6727] [7 8 6732] [1 6 6734] [9 7 6734] [1 7 6741] [0 4 6743] [0 6 6749] [4 6 6749] [9 8 6753] [10 10 6755] [4 5 6757] [6 8 6758] [10 8 6760] [2 7 6763] [10 6 6763] [8 7 6766] [7 9 6769] [3 6 6771] [0 7 6777] [1 10 6777] [7 7 6779] [3 9 6783] [4 7 6786] [7 10 6787] [0 5 6794] [1 9 6796] [10 7 6803] [4 4 6823] [1 4 6824] [1 8 6827] [2 10 6829] [2 6 6847] [10 9 6852] [0 10 6859] [0 9 6863] [5 6 6876] [0 8 6893] [2 8 6895] [3 5 6901] [1 0 7971] [2 0 8009] [0 0 8152] [3 0 8644] [4 0 9535] [5 0 10000] [6 0 10000] [7 0 10000] [8 0 10000] [9 0 10000] [10 0 10000])


  )
