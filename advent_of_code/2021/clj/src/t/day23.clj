(ns t.day23)

(def mapping
  (->> [[0 0] [1 0]       [3 0]       [5 0]       [7 0]       [9 0] [10 0]
                    [2 1]       [4 1]       [6 1]       [8 1]
                    [2 2]       [4 2]       [6 2]       [8 2]
                    [2 3]       [4 3]       [6 3]       [8 3]
                    [2 4]       [4 4]       [6 4]       [8 4]]
       (map-indexed (fn [idx v] [v idx]))
       (into {})))

(defn sign
  [^longs arr]
  (let [l ^int (alength arr)
        stop ^int (unchecked-dec-int l)
        ^long new-sign (loop [i (int 0)
                              s 0]
                         (if (== i stop)
                           s
                           (recur (inc i)
                                  (+ (* 5 s)
                                     (case (aget arr i)
                                       0 0, 1 1, 10 2, 100 3, 1000 4)))))]
    (aset arr (dec l) new-sign)
    arr))

(defn parse
  [lines]
  (->> lines
       (drop 2)
       (take 2)
       (map-indexed (fn [y line]
                      (map-indexed (fn [x c]
                                     (when-let [c ({\A 1 \B 10 \C 100 \D 1000} c)]
                                       [[(- x 1) (inc y)] c]))
                                   line)))
       (apply concat)
       (remove nil?)
       (reduce (fn [^longs arr [k ^long v]]
                 (aset arr (mapping k) v)
                 arr)
               (make-array Long/TYPE 16))
       (sign)))

(def ^"[[J" adj-arr-1
  (->> {0 [1]
        1 [0 2 7]
        2 [1 7 8 3]
        3 [2 8 9 4]
        4 [3 9 10 5]
        5 [4 10 6]
        6 [5]
        7 [1 2 11]
        8 [2 3 12]
        9 [3 4 13]
        10 [4 5 14]
        11 [7]
        12 [8]
        13 [9]
        14 [10]}
       (reduce (fn [^"[[J" arr [k vs]]
                 (aset arr ^long k ^longs (into-array Long/TYPE vs))
                 arr)
               (make-array Long/TYPE 15 0))))

(def ^"[[J" adj-arr-2
  (->> {0 [1]
        1 [0 2 7]
        2 [1 7 8 3]
        3 [2 8 9 4]
        4 [3 9 10 5]
        5 [4 10 6]
        6 [5]
        7 [1 2 11]
        8 [2 3 12]
        9 [3 4 13]
        10 [4 5 14]
        11 [7 15]
        12 [8 16]
        13 [9 17]
        14 [10 18]
        15 [11 19]
        16 [12 20]
        17 [13 21]
        18 [14 22]
        19 [15]
        20 [16]
        21 [17]
        22 [18]}
       (reduce (fn [^"[[J" arr [k vs]]
                 (aset arr ^long k ^longs (into-array Long/TYPE vs))
                 arr)
               (make-array Long/TYPE 23 0))))

(def end-state {2 1, 4 10, 6 100, 8 1000})

(let [d (->> mapping
             (map (fn [[k v]] [v k]))
             sort
             (map (fn [[k v]] v))
             into-array)
      l (alength ^"[Ljava.lang.Object;" d)]
  (defn decode
    [^long n]
    (when (< n l)
      (aget ^"[Ljava.lang.Object;" d n))))

(defn abs
  [n]
  (if (neg? n) (- n) n))

(let [m (->> (for [from-x (range 11)
                   from-y (range 5)
                   :let [from-idx (mapping [from-x from-y])]
                   :when from-idx
                   to-x (range 11)
                   to-y (range 5)
                   :let [to-idx (mapping [to-x to-y])]
                   :when to-idx]
               [from-idx to-idx
                (if (zero? (* from-y to-y))
                  (+ (abs (- from-x to-x))
                     (abs (- from-y to-y)))
                  (+ from-y to-y (abs (- from-x to-x))))])
             (reduce (fn [acc [from to d]]
                       (assoc acc [from to] d))
                     {}))]
  (defn dist
    [k] (m k)))

(defn move-amphi
  [^longs state ^long from-idx ^long to-idx]
  (let [ret ^longs (aclone state)]
    [(* (aget state from-idx)
        (dist [from-idx to-idx]))
     (sign (doto ^longs (aclone state)
             (aset from-idx 0)
             (aset to-idx (aget state from-idx))))]))

(defn final-position?
  [^long pos ^long cost ^longs amphipods]
  (and (>= pos 7)
       (let [t-pos (case cost 1 7, 10 8, 100 9, 1000 10)]
         (if (== 16 (alength amphipods))
           (or (== pos (+ t-pos 4))
               (and (== pos t-pos)
                    (== cost (aget amphipods (+ 4 t-pos)))))
           (or (== pos (+ t-pos 12))
               (and (== pos (+ t-pos 8))
                    (== cost (aget amphipods (+ 12 t-pos))))
               (and (== pos (+ t-pos 4))
                    (== cost (aget amphipods (+ 8 t-pos)))
                    (== cost (aget amphipods (+ 12 t-pos))))
               (and (== pos t-pos)
                    (== cost (aget amphipods (+ 4 t-pos)))
                    (== cost (aget amphipods (+ 8 t-pos)))
                    (== cost (aget amphipods (+ 12 t-pos)))))))))

(defn possible-moves
  [^longs amphipods ^"[[J" adjacent]
  (let [limit (dec (alength amphipods))
        mt-q clojure.lang.PersistentQueue/EMPTY]
    (loop [pos 0
           ret ()
           visited #{0}
           poss (conj mt-q 0)]
      (let [cost (aget amphipods pos)]
        (cond (== pos limit)
              ret
              (or (zero? cost)
                  (empty? poss)
                  (final-position? pos cost amphipods))
              (recur (inc pos) ret #{(inc pos)} [(inc pos)])
              :else
              (let [e-pos (peek poss)
                    poss (transduce (filter (fn [adj]
                                              (and (not (visited adj))
                                                   (zero? (aget amphipods ^long adj))
                                                   (or (< adj 7)
                                                       (> adj 10)
                                                       (let [t ({7 1, 8 10, 9 100, 10 1000} adj)]
                                                         (or (== pos (+ adj 4))
                                                             (== pos (+ adj 8))
                                                             (== pos (+ adj 12))
                                                             (and (== cost t)
                                                                  (let [c (get amphipods (+ 4 adj))]
                                                                    (or (== t c) (zero? c)))
                                                                  (or (== 15 limit)
                                                                      (let [c2 (aget amphipods (+ 8 adj))
                                                                            c3 (aget amphipods (+ 12 adj))]
                                                                        (and (or (== t c2) (zero? c2))
                                                                             (or (== t c3) (zero? c3))))))))))))
                                    conj (pop poss) (aget adjacent ^long e-pos))
                    valid? (not (or
                                  ;; we've already reached this one
                                  (visited e-pos)
                                  ;; can't start in hallway, end in hallway
                                  (and (< pos 7) (< e-pos 7))
                                  ;; can't stop in room with space beneath
                                  (let [beneath (+ e-pos 4)]
                                    (and (>= e-pos 7)
                                         (< beneath limit)
                                         (seq (aget adjacent ^long beneath))
                                         (or (== pos beneath)
                                             (zero? (aget amphipods beneath)))))))
                    visited (conj visited e-pos)]
                (cond (and valid? (> e-pos 6))
                      [(move-amphi amphipods pos e-pos)]
                      valid?
                      (recur pos (conj ret (move-amphi amphipods pos e-pos)) visited poss)
                      :else
                      (recur pos ret visited poss))))))))

(comment

  (def sample
    (into-array Long/TYPE [0 0 0 0 0 0 0 10 100 10 1000 1 1000 100 1 1060580]))

  )

(defn heuristic
  [^longs state]
  (let [limit (dec (alength state))]
    (loop [pos 0
           h 0]
      (if (== limit pos)
        h
        (recur (inc pos)
               (long (let [c (aget state pos)]
                       (if (zero? c)
                         h
                         (let [x (case c 1 7, 10 8, 100 9, 1000 10)]
                           (if (or (== x pos)
                                   (== (+ 4 x) pos)
                                   (== (+ 8 x) pos)
                                   (== (+ 12 x) pos))
                             h
                             (+ h (* c (dist [pos x])))))))))))))

(defn final-state?
  [^longs state]
  (let [stop (dec (alength state))]
    (loop [idx 7]
      (cond (>= idx stop)
            true
            (and (== 1 (aget state idx))
                 (== 10 (aget state (+ idx 1)))
                 (== 100 (aget state (+ idx 2)))
                 (== 1000 (aget state (+ idx 3))))
            (recur (+ idx 4))
            :else
            false))))

(defn solve
  [^longs input ^"[[J" adjacency]
  (let [s (- (alength input) 1)]
    (loop [to-process (sorted-map (heuristic input) (list [input 0]))
           visited {}
           i 0]
      (if (empty? to-process)
        :error
        (let [[min-h [[^longs state cost-to-reach] & r]] (first to-process)
              to-process (if (seq r)
                           (assoc to-process min-h r)
                           (dissoc to-process min-h))]
          (cond (when-let [v (visited (aget state s))]
                  (<= v cost-to-reach))
                (recur to-process visited (inc i))
                (final-state? state)
                #_(do (prn [:iters i]) cost-to-reach)
                cost-to-reach
                :else
                (recur (let [pm (possible-moves state adjacency)]
                         (reduce (fn [tp move]
                                   (let [cost-of-move (get move 0)
                                         state-after-move (get move 1)
                                         total-cost (+ cost-to-reach cost-of-move)]
                                     (update tp
                                             (+ (heuristic state-after-move) total-cost)
                                             (fnil conj ())
                                             [state-after-move total-cost])))
                                 to-process
                                 pm))
                       (assoc visited (last state) cost-to-reach)
                       (inc i))))))))

(defn part1
  [input]
  (solve input adj-arr-1))

(defn part2
  [^longs input]
  (solve (let [arr ^"[J" (make-array Long/TYPE 24)]
           (loop [i 0]
             (when (< i 11)
               (aset arr i (aget input i))
               (recur (inc i))))
           (reduce (fn [^"[J" arr [k ^long t]]
                     (aset arr ^long (mapping k) t)
                     arr)
                   arr
                   [[[2 2] 1000] [[2 3] 1000] [[4 2] 100] [[4 3] 10]
                    [[6 2] 10] [[6 3] 1] [[8 2] 1] [[8 3] 100]])
           (aset arr 19 (aget input 11))
           (aset arr 20 (aget input 12))
           (aset arr 21 (aget input 13))
           (aset arr 22 (aget input 14))
           (sign arr))
         adj-arr-2))
