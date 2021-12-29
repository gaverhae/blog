(ns t.day23
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

(def mapping
  (->> [[0 0] [1 0]       [3 0]       [5 0]       [7 0]       [9 0] [10 0]
                    [2 1]       [4 1]       [6 1]       [8 1]
                    [2 2]       [4 2]       [6 2]       [8 2]
                    [2 3]       [4 3]       [6 3]       [8 3]
                    [2 4]       [4 4]       [6 4]       [8 4]]
       (map-indexed (fn [idx v] [v idx]))
       (into {})))

(defnp sign
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
               (make-array Long/TYPE 23 0))))

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

(defnp abs
  [n]
  (if (neg? n) (- n) n))

(def dist
  (->> (for [from-x (range 11)
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
               {})))

(defnp move-amphi
  [^longs state ^long from-idx ^long to-idx]
  (let [ret ^longs (aclone state)]
    [(* (aget state from-idx)
        (dist [from-idx to-idx]))
     (sign (doto ^longs (aclone state)
             (aset from-idx 0)
             (aset to-idx (aget state from-idx))))]))

(defmacro final-position?
  [pos cost amphipods]
  `(case ~cost
     ~@(->> [[1 7] [10 8] [100 9] [1000 10]]
            (mapcat (fn [[target-cost target-pos]]
                      [target-cost
                       `(case ~pos
                          ~target-pos (and (== ~target-cost (aget ~amphipods ~(+ 4 target-pos)))
                                           (or (== 16 (alength ~amphipods))
                                               (and (== ~target-cost (aget ~amphipods ~(+ 8 target-pos)))
                                                    (== ~target-cost (aget ~amphipods ~(+ 12 target-pos))))))
                          ~(+ 4 target-pos) (or (== 16 (alength ~amphipods))
                                                (and (== ~target-cost (aget ~amphipods ~(+ 8 target-pos)))
                                                     (== ~target-cost (aget ~amphipods ~(+ 12 target-pos)))))
                          ~(+ 8 target-pos) (== ~target-cost (aget ~amphipods ~(+ 12 target-pos)))
                          ~(+ 12 target-pos) true
                          false)])))
     false))

(defnp possible-moves
  [^longs amphipods ^"[[J" adjacent]
  (loop [pos 0
         ret ()]
    (let [cost (aget amphipods pos)
          [start-x] (decode pos)]
      (cond (== pos (dec (alength amphipods)))
            ret
            (or (zero? cost)
                (p :already-good (final-position? pos cost amphipods)))
            (recur (inc pos) ret)
            :else
            (recur (inc pos)
                   (loop [poss [pos]
                          visited #{pos}
                          reachable ret]
                     (if (empty? poss)
                       reachable
                       (let [e-pos (first poss)]
                         (recur (concat (rest poss)
                                        (->> (aget adjacent ^long e-pos)
                                             (remove visited)
                                             (remove (fn [k]
                                                       (pos? (aget amphipods ^long k))))
                                             (map decode)
                                             (remove (fn entered-wrong-room
                                                       [[adj-x adj-y]]
                                                       (p :entered-wrong-room
                                                          (and (not= adj-x start-x)
                                                               (>= adj-y 1)
                                                               (not= cost (end-state adj-x))))))
                                             (remove (fn wrong-amphi-in-room
                                                       [[adj-x adj-y]]
                                                       (p :wrong-amphi-in-room
                                                          (let [k2 (mapping [adj-x 2])
                                                                k3 (mapping [adj-x 3])
                                                                k4 (mapping [adj-x 4])]
                                                            (and (not= adj-x start-x)
                                                                 (== 1 adj-y)
                                                                 (or (and (pos? (aget amphipods k2))
                                                                          (not= (aget amphipods k2) cost))
                                                                     (and (< k3 (dec (alength amphipods)))
                                                                          (pos? (aget amphipods k3))
                                                                          (not= (aget amphipods k3) cost))
                                                                     (and (< k4 (dec (alength amphipods)))
                                                                          (pos? (aget amphipods k4))
                                                                          (not= (aget amphipods k4) cost))))))))
                                             (map mapping)))
                                (conj visited e-pos)
                                (if (p :check-end-pos
                                       (or
                                         ;; we've already reached this one
                                         (visited e-pos)
                                         ;; can't start in hallway, end in hallway
                                         (and (< pos 7) (< e-pos 7))
                                         ;; can't stop in room with space beneath
                                         (let [beneath (+ e-pos 4)]
                                           (and (>= e-pos 7)
                                                (< beneath (dec (alength amphipods)))
                                                (seq (aget adjacent ^long beneath))
                                                (or (== pos beneath)
                                                    (zero? (aget amphipods beneath)))))))
                                  reachable
                                  (conj reachable (move-amphi amphipods pos e-pos))))))))))))

(comment

  (def sample
    (into-array Long/TYPE [0 0 0 0 0 0 0 10 100 10 1000 1 1000 100 1 1060580]))

  )

(defnp heuristic
  [state]
  (->> state
       butlast
       (map-indexed (fn [idx t] [(decode idx) t]))
       (remove (fn [[_ t]] (zero? t)))
       (map (fn [[[x y] c]]
              (let [dx (abs (- x ({1 2, 10 4, 100 6, 1000 8} c)))]
                (if (zero? dx)
                  0
                  (* c (+ dx y 1))))))
       (reduce +)))

(defn print-state
  [^longs state]
  (let [p (fn [pos]
            (get {1 "A" 10 "B" 100 "C" 1000 "D" 0 "."} (aget state ^long (mapping pos))))]
    (prn [:seq (map-indexed vector state)])
    (println "#############")
    (print "#")
    (doseq [n (range 11)]
      (print (p [n 0])))
    (println "#")
    (loop [i 11]
      (when (< i (dec (alength state)))
        (println (format "  #%s#%s#%s#%s#  "
                         ({1 "A" 10 "B" 100 "C" 1000 "D" 0 "."} (aget state (+ 0 i)))
                         ({1 "A" 10 "B" 100 "C" 1000 "D" 0 "."} (aget state (+ 1 i)))
                         ({1 "A" 10 "B" 100 "C" 1000 "D" 0 "."} (aget state (+ 2 i)))
                         ({1 "A" 10 "B" 100 "C" 1000 "D" 0 "."} (aget state (+ 3 i)))))
        (recur (+ i 4))))
    (println         "  #########  ")))

(defnp solve
  [^longs input ^"[[J" adjacency]
  #_(print-state input)
  (loop [to-process (sorted-map (heuristic input) (list [input 0]))
         visited {}
         i 0]
    (if (empty? to-process)
      :error
      (let [[min-h [[^longs state cost-to-reach] & r]] (first to-process)
            to-process (if (empty? r)
                         (dissoc to-process min-h)
                         (assoc to-process min-h r))]
        #_(when (zero? (mod i 100000))
          (print-state state)
          (prn [:cost cost-to-reach :h min-h :i i]))
        (cond (p :filter-visited
                 (when-let [v (visited (last state))]
                   (<= v cost-to-reach)))
              (recur to-process visited (inc i))
              (p :end-state?
                 (->> state
                      butlast
                      (map-indexed (fn [idx t] [(decode idx) t]))
                      (remove (fn [[pos t]] (zero? t)))
                      (every? (fn [[[x y] c]]
                                (and (pos? y)
                                     (== c (end-state x)))))))
              cost-to-reach
              :else
              (recur (let [pm (possible-moves state adjacency)]
                       (p :next-states
                          (reduce (fn [tp move]
                                    (let [cost-of-move (get move 0)
                                          state-after-move (get move 1)
                                          total-cost (+ cost-to-reach cost-of-move)]
                                      (update tp
                                              (+ (heuristic state-after-move) total-cost)
                                              (fnil conj ())
                                              [state-after-move total-cost])))
                                    to-process
                                    pm)))
                     (assoc visited (last state) cost-to-reach)
                     (inc i)))))))

(defn part1
  [input]
  (p :part1 (solve input adj-arr-1)))

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
