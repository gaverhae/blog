(ns t.day15
  (:require [clojure.set :as set])
  (:import [java.util PriorityQueue]))

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
    (loop [score {start 0}
           open-set {}
           cur-pos start]
      (if (= cur-pos target)
        (score cur-pos)
        (let [neighs (->> (neighbours cur-pos)
                          set
                          (keep (fn [p]
                                  (when-let [c (cost-of-entering p)]
                                    (let [new-score (+ (score cur-pos) c)]
                                      (when (< new-score (score p Long/MAX_VALUE))
                                        [p new-score (+ new-score (h p))]))))))]
          (recur (reduce (fn [score [n g _]]
                           (assoc score n g))
                         score neighs)
                 (reduce (fn [open-set [n _ f]]
                           (when-let [old-f (open-set n)]
                             (.remove pq [old-f n]))
                           (.add pq [f n])
                           (assoc open-set n f))
                         open-set neighs)
                 (second (.poll pq))))))))

(defn part1
  [input]
  (dijkstra input))

(defn part2
  [{:keys [width height costs]}]
  (let [increment {1 2, 2 3, 3 4, 4 5, 5 6, 6 7, 7 8, 8 9, 9 1}]
    (a* {:width (* 5 width)
         :height (* 5 height)
         :costs (->> costs
                     (mapcat (fn [[[x y] v]]
                               (for [dx (range 5)
                                     dy (range 5)]
                                 [[(+ x (* dx width))
                                   (+ y (* dy height))]
                                  (nth (iterate increment v) (+ dx dy))])))
                     (into {}))})))
