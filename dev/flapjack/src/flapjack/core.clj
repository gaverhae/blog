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

(defprotocol Dealer
  (start-game [_] "Returns a token for a new game.")
  (draw [_ token] "Returns the next card.")
  (stick [_ token score] "Ends the round and declare the client's score.")
  (end-game [_ token winner] "Ends the game and declares the winner."))

(defn in-memory-dealer
  "Note: not thread-safe."
  ([] (in-memory-dealer (atom {})))
  ([state]
    (reify Dealer
      (start-game [_]
        (let [game-id (.toString (UUID/randomUUID))]
          (swap! state assoc game-id {:rounds-played 0
                                      :rounds [{:taken 0
                                                :deck (half-deck)}]})
          [:ok game-id]))
      (draw [_ game-id]
        (let [game-state (get @state game-id)
              {:keys [taken, deck]} (-> game-state :rounds last)]
          (if (and (< (:rounds-played game-state) 5)
                   (or (zero? taken)
                       (not (busted? (take taken deck))))
                   (< taken (count deck)))
            (do (swap! state update-in [game-id :rounds (:rounds-played game-state) :taken] inc)
                [:ok (get deck taken)])
            ;; trying to start a 6th round, trying to pull more cards than in
            ;; deck, or trying to keep drawing when score >25.
            [:error "Invalid command for current state."])))
      (stick [_ game-id client-points]
        ;; FIXME: server needs to play too
        (let [server (unattended-round (fn [_] (> (rand) 0.8)) (half-deck))
              {:as game-state, :keys [rounds-played rounds]} (get @state game-id)
              {:keys [taken, deck]} (last rounds)]
          (if (and (< (:rounds-played game-state) 5)
                   (>= taken 1)
                   (contains? (set (points (take taken deck))) client-points))
            (do (swap! state update game-id #(-> %
                                                 (update :rounds-played inc)
                                                 (assoc-in [:rounds rounds-played :client-points] client-points)
                                                 (assoc-in [:rounds rounds-played :server] server)))
                [:ok (get-in @state [game-id :rounds rounds-played :server 1])])
            [:error "Invalid command for current state."])))
      (end-game [_ game-id winner-claim]
        (let [{:as game-state, :keys [rounds-played rounds]} (get @state game-id)
              {:keys [taken, deck]} (last rounds)
              client-score (->> rounds
                                (map :client-points)
                                (reduce +))
              server-score (->> rounds (map :server-points) (reduce +))
              winner (cond (> client-score server-score) :server
                           (< client-score server-score) :client
                           :else :draw)]
          (if (and (= rounds-played 5)
                   (= winner winner-claim))
            [:ok]
            [:error "Invalid command for current state."]))))))

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

(comment
(time (dotimes [_ 10000] (half-deck)))
(with-out-str (time (dotimes [_ 10000]
        (best-play (half-deck)))))
"\"Elapsed time: 12456.804358 msecs\"\n"
(with-out-str (time (dotimes [_ 10000]
        (best-play (half-deck)))))
"\"Elapsed time: 459.123759 msecs\"\n"
)

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

(defn previous-best-play
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

(comment

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



(comment

  (def dealer-state (atom {}))
  (def d (in-memory-dealer dealer-state))
  (def h (atom []))
  (def t (second (start-game d)))

  (-> t)
  (-> @dealer-state)
  (-> @h)

  (let [[ok? card] (draw d t)] (if (= :ok ok?) (do (swap! h conj card) [@h (best-points @h) (count @h)]) [ok? card]))

  (stick d t (best-points @h))

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
(test-strategy-oracle
  (fn [deck]
    (strat-take-n (previous-best-play deck)))
  10000)
44900

(test-strategy (monte-carlo-ish 100) 100)
902
100
96

  )
