(ns t.day15
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [clojure.set :as set])
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
        target [(dec (count (first input)))
                (dec (count input))]]
    (loop [[cost pos] [0 [0 0]]
           unvisited (dissoc (->> cost-of-entering
                                  (map (fn [[k v]] [k Long/MAX_VALUE]))
                                  (into {}))
                             [0 0])
           pq ^PriorityQueue (reduce (fn [^PriorityQueue pq [pos cost]]
                                       (.add pq [cost pos])
                                       pq)
                                     (PriorityQueue. (count cost-of-entering) compare)
                                     unvisited)]
      (if (= pos target)
        cost
        (let [neighs (->> (neighbours pos)
                          (filter unvisited))
              new-unvisited (reduce (fn [unvisited neighbour]
                                      (update unvisited
                                              neighbour
                                              (fn [old]
                                                (min old (+ cost
                                                            (cost-of-entering neighbour))))))
                                    (dissoc unvisited pos)
                                    neighs)]
          (doseq [n neighs]
            (.remove pq [(unvisited n) n])
            (.add pq [(new-unvisited n) n]))
          (recur (.poll pq)
                 new-unvisited
                 pq))))))

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
  (a* input))

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
    (a* expanded)))
