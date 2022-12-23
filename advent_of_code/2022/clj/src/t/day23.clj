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

(defn print-board
  [positions]
  (let [[xmin xmax ymin ymax] (reduce (fn [[xmin xmax ymin ymax] [y x]]
                                        [(min xmin x)
                                         (max xmax x)
                                         (min ymin y)
                                         (max ymax y)])
                                      [(get (first (keys positions)) 1)
                                       (get (first (keys positions)) 1)
                                       (get (first (keys positions)) 0)
                                       (get (first (keys positions)) 0)]
                                      (rest (keys positions)))]
    (doseq [y (range ymin (inc ymax))]
      (println) (flush)
      (doseq [x (range xmin (inc xmax))]
        (print (format "%3s" (str (positions [y x] " . "))))))
    (println) (flush)))

(defn part1
  [input]
  (loop [n 0
         directions 0
         positions (->> input (map-indexed (fn [idx p] [p idx]))
                        (into {}))]
    (prn [n positions directions (count positions)])
    (print-board positions)
    (if (== 10 n)
      (let [[xmin xmax ymin ymax] (reduce (fn [[xmin xmax ymin ymax] [y x]]
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
                 1)))
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
                                                             (prn [:look pos move looks (map positions looks)])
                                                             (when (every? nil? (map positions looks))
                                                               move))))]
                                       (prn [:prop pos prop])
                                       (assoc acc (val pos) prop)))
                                   {}))
            conflicts (->> proposals
                           vals
                           (remove nil?)
                           frequencies
                           (filter (fn [[k v]] (>= v 2)))
                           keys
                           set)]
        (recur (inc n)
               (mod (inc directions) 4)
               (->> positions
                    (map (fn [[p idx]]
                           (let [to (proposals idx)]
                             (if (and to (not (conflicts to)))
                               [to idx]
                               [p idx]))))
                    (into {})))))))


(defn part2
  [input]
  (loop [n 0
         directions 0
         positions (->> input (map-indexed (fn [idx p] [p idx]))
                        (into {}))]
    (prn [n positions directions (count positions)])
    (print-board positions)
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
                                                           (prn [:look pos move looks (map positions looks)])
                                                           (when (every? nil? (map positions looks))
                                                             move))))]
                                     (prn [:prop pos prop])
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
                             (into {}))]
      (if (= new-positions positions)
        (inc n)
        (recur (inc n)
               (mod (inc directions) 4)
               new-positions)))))

(lib/check
  [part1 sample] 110
  [part1 puzzle] 3996
  [part2 sample] 20
  [part2 puzzle] 908)
