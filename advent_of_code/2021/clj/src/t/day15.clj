(ns t.day15
  (:require [clojure.set :as set])
  (:import [java.util PriorityQueue]))

(def i (atom 0))

(defn parse
  [lines]
  {:width (count (first lines))
   :height (count lines)
   :costs (->> lines
               (map-indexed (fn [y line]
                              (map-indexed (fn [x cost]
                                             [[x y] (Long/parseLong (str cost))])
                                           line)))
               (apply concat)
               (into {}))})

(defn neighbours
  [[x y]]
  #{[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]})

(defn search
  [initial final? generate-moves]
  (let [INF Long/MAX_VALUE
        put-in (fn [m [k v]] (update m k (fnil min INF) v))]
    (loop [[state cost :as current] [initial 0]
           to-visit {}
           visited {}]
      (do (swap! i inc)
          (when (zero? (rem @i 10000))
            (prn [:to-visit (count to-visit) :visited (count visited) :total (->> visited (map val) (reduce +))])))
      (let [to-visit (if (<= (visited state INF) cost)
                       to-visit
                       (->> (generate-moves current)
                            (remove (fn [[nxt-state nxt-cost]]
                                      (>= nxt-cost (visited nxt-state INF))))
                            (reduce put-in
                                    to-visit)))
            visited (put-in visited current)]
        (if (empty? to-visit)
          (->> visited
               (filter (fn [[state cost]] (final? state)))
               (map (fn [[state cost]] cost))
               (reduce min))
          (recur (first to-visit)
                 (into {} (rest to-visit))
                 visited))))))

(defn depth-first-search
  [initial final? generate-moves]
  (let [INF Long/MAX_VALUE
        put-in (fn [m [k v]] (update m k (fnil min INF) v))]
    (loop [[state cost :as current] [initial 0]
           to-visit () ;; changed to a list
           visited {}]
      (do (swap! i inc)
          (when (zero? (rem @i 1000000))
            (prn [:to-visit (count to-visit) :visited (count visited) :total (->> visited (map val) (reduce +))])))
      (let [to-visit (if (<= (visited state INF) cost)
                       to-visit
                       (->> (generate-moves current)
                            (remove (fn [[nxt-state nxt-cost]]
                                      (>= nxt-cost (visited nxt-state INF))))
                            (reduce conj ;; changed `put-in` to `conj`
                                    to-visit)))
            visited (put-in visited current)]
        (if (empty? to-visit)
          (->> visited
               (filter (fn [[state cost]] (final? state)))
               (map (fn [[state cost]] cost))
               (reduce min))
          (recur (first to-visit)
                 (rest to-visit) ;; no need to rebuild a map anymore
                 visited))))))

(defn breadth-first-search
  [initial final? generate-moves]
  (let [INF Long/MAX_VALUE
        put-in (fn [m [k v]] (update m k (fnil min INF) v))]
    (loop [[state cost :as current] [initial 0]
           to-visit clojure.lang.PersistentQueue/EMPTY ;; changed
           visited {}]
      (do (swap! i inc)
          (when (zero? (rem @i 10000))
            (prn [:to-visit (count to-visit) :visited (count visited) :total (->> visited (map val) (reduce +))])))
      (let [to-visit (if (<= (visited state INF) cost)
                       to-visit
                       (->> (generate-moves current)
                            (remove (fn [[nxt-state nxt-cost]]
                                      (>= nxt-cost (visited nxt-state INF))))
                            (reduce conj ;; changed `put-in` to `conj`
                                    to-visit)))
            visited (put-in visited current)]
        (if (empty? to-visit)
          (->> visited
               (filter (fn [[state cost]] (final? state)))
               (map (fn [[state cost]] cost))
               (reduce min))
          (recur (peek to-visit) ;; gets the "least recently added" item
                 (pop to-visit) ;; returns queue without (peek q)
                 visited))))))

(defn dijkstra-search
  [initial final? generate-moves]
  (let [to-visit (java.util.PriorityQueue. 100 compare)]
    (loop [[cost state] [0 initial]
           visited #{}]
      (when (not (visited state))
        (doseq [[nxt-state nxt-cost] (generate-moves [state cost])]
          (when (not (visited nxt-state))
            (.add to-visit [nxt-cost nxt-state]))))
      (if (final? state)
        cost
        (recur (.poll to-visit)
               (conj visited state))))))

(defn dijkstra-search-imm
  [initial final? generate-moves]
  (loop [[cost states] [0 [initial]]
         to-visit (sorted-map 0 [initial])
         visited #{}]
    (let [to-visit (->> states
                        (remove visited)
                        (mapcat (fn [state] (generate-moves [state cost])))
                        (reduce (fn [m [s c]]
                                  (if (visited s)
                                    m
                                    (update m c (fnil conj #{}) s)))
                                (dissoc to-visit cost)))]
      (if (not (every? (complement final?) states))
        cost
        (recur (first to-visit)
               to-visit
               (reduce conj visited states))))))

(defn a-star-search
  [initial final? generate-moves heuristic]
  (let [to-visit (java.util.PriorityQueue. 100 compare)]
    (loop [[guess cost state] [(heuristic initial) 0 initial]
           visited #{}]
      (when (not (visited state))
        (doseq [[nxt-state nxt-cost] (generate-moves [state cost])]
          (when (not (visited nxt-state))
            (.add to-visit [(+ nxt-cost (heuristic nxt-state))
                            nxt-cost nxt-state]))))
      (if (final? state)
        cost
        (recur (.poll to-visit)
               (conj visited state))))))

(defn dijkstra
  [{cost-of-entering :costs :keys [width height]}]
  (let [start [0 0]
        target [(dec width)
                (dec height)]]
    (loop [frontier {0 #{start}}
           visited #{start}
           distance 0]
      (let [nodes-to-process (frontier distance)]
        (if (contains? nodes-to-process target)
          distance
          (recur (->> nodes-to-process
                      (mapcat neighbours)
                      (remove visited)
                      set
                      (keep (fn [n]
                              (when-let [c (cost-of-entering n)]
                                [n (+ distance c)])))
                      (reduce (fn [f [n c]]
                                (update f c (fnil conj #{}) n))
                              frontier))
                 (set/union nodes-to-process visited)
                 (inc distance)))))))

(defn a*
  [{cost-of-entering :costs :keys [width height]}]
  (let [start [0 0]
        target [(dec width) (dec height)]
        h (fn [[x y]]
            (+ (- (first target) x)
               (- (second target) y)))
        pq (PriorityQueue. 100 compare)]
    (loop [score {start [0 (h start)]}
           cur-pos start]
      (if (= cur-pos target)
        (first (score cur-pos))
        (let [neighs (->> (neighbours cur-pos)
                          set
                          (keep (fn [p]
                                  (when-let [c (cost-of-entering p)]
                                    (let [new-score (+ (first (score cur-pos)) c)]
                                      (when (< new-score (first (score p [Long/MAX_VALUE])))
                                        [p new-score (+ new-score (h p))]))))))]
          (doseq [[n _ f] neighs]
            (when-let [[_ old-f] (score n)]
              (.remove pq [old-f n]))
            (.add pq [f n]))
          (recur (reduce (fn [score [n g f]]
                           (assoc score n [g f]))
                         score neighs)
                 (second (.poll pq))))))))

(defn part1
  [input]
  #_(dijkstra input)
  #_(a* input)
  (let [generate-moves (fn [[pos cost-so-far]]
                         (->> (neighbours pos)
                              (keep (fn [p]
                                      (when-let [c ((:costs input) p)]
                                        [p (+ cost-so-far c)])))
                              set))
        initial [0 0]
        final? (fn [[x y]] (and (== x (dec (:width input)))
                                (== y (dec (:height input)))))
        heuristic (fn [[x y]]
                    (+ (- (dec (:width input)) x)
                       (- (dec (:height input)) y)))]
    #_(search initial final? generate-moves)
    #_(depth-first-search initial final? generate-moves)
    #_(breadth-first-search initial final? generate-moves)
    #_(dijkstra-search initial final? generate-moves)
    (dijkstra-search-imm initial final? generate-moves)
    #_(a-star-search initial final? generate-moves heuristic)))

(defn part2
  [{:keys [width height costs]}]
  (let [increment {1 2, 2 3, 3 4, 4 5, 5 6, 6 7, 7 8, 8 9, 9 1}]
    (part1 {:width (* 5 width)
            :height (* 5 height)
            :costs (->> costs
                        (mapcat (fn [[[x y] v]]
                                  (for [dx (range 5)
                                        dy (range 5)]
                                    [[(+ x (* dx width))
                                      (+ y (* dy height))]
                                     (nth (iterate increment v) (+ dx dy))])))
                        (into {}))})))
