(ns t.day15
  (:require [clojure.set :as set])
  (:import [java.util PriorityQueue]))

(defn parse
  [lines]
  (->> lines
       (mapv (fn [line]
              (mapv (fn [c] (Long/parseLong (str c))) line)))))

(defn neighbours
  [[x y]]
  #{[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]})

(defn shortest-path
  [input]
  (let [cost-of-entering (->> input
                              (map-indexed (fn [y line]
                                             (map-indexed (fn [x cost]
                                                            [[x y] cost])
                                                          line)))
                              (apply concat)
                              (into {}))
        start [0 0]
        target [(dec (count (first input)))
                (dec (count input))]]
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
  [input]
  (let [cost-of-entering (->> input
                              (map-indexed (fn [y line]
                                             (map-indexed (fn [x cost]
                                                            [[x y] cost])
                                                          line)))
                              (apply concat)
                              (into {}))
        start [0 0]
        max-y (count input)
        max-x (count (first input))
        goal [(dec max-x)
              (dec max-y)]
        h (fn [[x y]]
            (+ (- (first goal) x)
               (- (second goal) y)))
        pq (PriorityQueue. 100 compare)]
    (loop [gscore {start 0}
           fscore {start (h start)}
           open-set {}
           cur-pos start]
      (if (= cur-pos goal)
        (gscore cur-pos)
        (let [neighs (->> (neighbours cur-pos)
                          (keep (fn [p]
                                  (when-let [c (cost-of-entering p)]
                                    (let [new-score (+ (gscore cur-pos) c)]
                                      (when (< new-score (gscore p Long/MAX_VALUE))
                                        [p new-score (+ new-score (h p))]))))))]
          (recur (reduce (fn [gscore [n g _]]
                           (assoc gscore n g))
                         gscore neighs)
                 (reduce (fn [fscore [n _ f]]
                           (assoc fscore n f))
                         fscore neighs)
                 (reduce (fn [open-set [n _ f]]
                           (when-let [old-f (open-set n)]
                             (.remove pq [old-f n]))
                           (.add pq [f n])
                           (assoc open-set n f))
                         open-set neighs)
                 (second (.poll pq))))))))

(defn part1
  [input]
  (shortest-path input))

(defn part2
  [input]
  (let [increment {1 2, 2 3, 3 4, 4 5, 5 6, 6 7, 7 8, 8 9, 9 1}
        expanded (->> input
                      (map (fn [line]
                             (->> line
                                  (iterate #(map increment %))
                                  (take 5)
                                  (apply concat)
                                  vec)))
                      (iterate (fn [lines]
                                 (mapv #(mapv increment %) lines)))
                      (take 5)
                      (apply concat)
                      vec)]
    (shortest-path expanded)))
