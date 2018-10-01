(ns t.core
  (:gen-class))

(defn deck
  []
  (for [c (range 4)
        i (range 1 14)]
    i))

(defn hand-value
  [cards]
  (->> cards
       (map (fn [c] (if (== 1 c)
                      11
                      (min 10 c))))
       (reduce + 0)))

(defn draw-until
  "Returns [final-hand final-deck]"
  ;; does not check for end of deck
  [initial-hand target deck]
  (if (or (empty? deck)
          (>= (hand-value initial-hand) target))
    [initial-hand deck]
    (recur (conj initial-hand (first deck))
           target
           (rest deck))))

(defn play-game
  "Returns :sam if sam wins, :dealer if dealer wins."
  [cards]
  (let [sam-initial-hand (take 2 cards)
        dealer-initial-hand (take 2 (drop 2 cards))
        deck-on-table (drop 4 cards)
        [sam-final-hand deck-on-table] (draw-until sam-initial-hand
                                                   17
                                                   deck-on-table)
        [dealer-final-hand deck-on-table] (draw-until dealer-initial-hand
                                                      (inc (hand-value sam-final-hand))
                                                      deck-on-table)]
    (cond ;; Ambiguous in problem statement; we decided Sam wins if both have 21
          (== 21 (hand-value sam-initial-hand))
          :sam

          (== 21 (hand-value dealer-initial-hand))
          :dealer

          (> (hand-value sam-final-hand) 21)
          :dealer

          ;; Problem statement ambiguous: we assume if Sam has 21
          ;; at this point they win
          (== 21 (hand-value sam-final-hand))
          :sam

          (> (hand-value dealer-final-hand) 21)
          :sam

          :else
          :dealer)))

(defn -main
  [& args]
  (play-game (shuffle (deck))))
