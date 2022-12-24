(ns t.day23
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (keep-indexed (fn [y line]
                      (->> line
                           (keep-indexed (fn [x p]
                                          (when (= p \#)
                                            [y x]))))))
       (apply concat)
       (into #{})))

(defn neighbours
  [[y x]]
  [[[(dec y) x] [[(dec y) x] [(dec y) (inc x)] [(dec y) (dec x)]]]
   [[(inc y) x] [[(inc y) x] [(inc y) (inc x)] [(inc y) (dec x)]]]
   [[y (dec x)] [[y (dec x)] [(inc y) (dec x)] [(dec y) (dec x)]]]
   [[y (inc x)] [[y (inc x)] [(inc y) (inc x)] [(dec y) (inc x)]]]])

(defn solve
  [input stop?]
  (loop [n 0
         directions 0
         positions (->> input (map-indexed (fn [idx p] [p idx]))
                        (into {}))]
    (let [proposals (->> positions
                         (map (fn [pos] [pos (->> (neighbours (key pos))
                                                  cycle
                                                  (drop directions)
                                                  (take 4))]))
                         (remove (fn [[pos neighs]]
                                   (->> neighs
                                        (mapcat (fn [[_ adj]] adj))
                                        (map positions)
                                        (every? nil?))))
                         (reduce (fn [acc [pos neighs]]
                                   (let [prop (->> neighs
                                                   (some (fn [[move looks]]
                                                           (when (every? nil? (map positions looks))
                                                             move))))]
                                     (assoc acc (val pos) prop)))
                                 {}))
          conflicts (->> proposals
                         vals
                         (remove nil?)
                         frequencies
                         (filter (fn [[k v]] (>= v 2)))
                         keys
                         set)
          new-positions (->> positions
                             (map (fn [[p idx]]
                                    (let [to (proposals idx)]
                                      (if (and to (not (conflicts to)))
                                        [to idx]
                                        [p idx]))))
                             (into {}))
          n (inc n)]
      (if (stop? n positions new-positions)
        [n new-positions]
        (recur n
               (mod (inc directions) 4)
               new-positions)))))

(defn part1
  [input]
  (let [[_ positions] (solve input (fn [n _ _] (== n 10)))
        [xmin xmax ymin ymax] (reduce (fn [[xmin xmax ymin ymax] [y x]]
                                        [(min xmin x)
                                         (max xmax x)
                                         (min ymin y)
                                         (max ymax y)])
                                      [(get (first (keys positions)) 1)
                                       (get (first (keys positions)) 1)
                                       (get (first (keys positions)) 0)
                                       (get (first (keys positions)) 0)]
                                      (rest (keys positions)))]
    (count (for [x (range xmin (inc xmax))
                 y (range ymin (inc ymax))
                 :when (not (positions [y x]))]
             1))))

(defn part2
  [input]
  (let [[n _] (solve input (fn [_ p1 p2] (= p1 p2)))]
    n))

(lib/check
  #_#_[part1 sample] 110
  #_#_[part1 puzzle] 3996
  #_#_[part2 sample] 20
  #_#_[part2 puzzle] 908)
