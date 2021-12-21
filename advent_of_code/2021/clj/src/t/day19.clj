(ns t.day19
  (:require [clojure.set :as set]))

(defn parse
  [lines]
  (let [scanner-line? (fn [l] (re-matches #"--- scanner \d+ ---" l))]
    (->> lines
         (partition-by scanner-line?)
         (remove (fn [g] (scanner-line? (first g))))
         (map #(remove #{""} %))
         (map #(mapv (fn [l]
                        (let [[_ x y z] (re-matches #"(-?\d+),(-?\d+),(-?\d+)" l)]
                          [(Long/parseLong x)
                           (Long/parseLong y)
                           (Long/parseLong z)]))
                      %))
         (mapv sort))))

(defn v-
  [[x0 y0 z0] [x1 y1 z1]]
  [(- x0 x1) (- y0 y1) (- z0 z1)])

(defn remap
  [probe v0]
  (->> probe (mapv #(v- % v0))))

(defn all-beacons-as-origin
  [probe]
  (->> probe
       (mapv (fn [v] (remap probe v)))))

(defn all-orientations
  [probe]
  (let [rotations [(fn [[x y z]] [(- x) (- y) z])
                   (fn [[x y z]] [(- x) (- z) (- y)])
                   (fn [[x y z]] [(- x) y (- z)])
                   (fn [[x y z]] [(- x) z y])
                   (fn [[x y z]] [(- y) (- x) (- z)])
                   (fn [[x y z]] [(- y) (- z) x])
                   (fn [[x y z]] [(- y) x z])
                   (fn [[x y z]] [(- y) z (- x)])
                   (fn [[x y z]] [(- z) (- x) y])
                   (fn [[x y z]] [(- z) (- y) (- x)])
                   (fn [[x y z]] [(- z) x (- y)])
                   (fn [[x y z]] [(- z) y x])
                   (fn [[x y z]] [x (- y) (- z)])
                   (fn [[x y z]] [x (- z) y])
                   (fn [[x y z]] [x y z])
                   (fn [[x y z]] [x z (- y)])
                   (fn [[x y z]] [y (- x) z])
                   (fn [[x y z]] [y (- z) (- x)])
                   (fn [[x y z]] [y x (- z)])
                   (fn [[x y z]] [y z x])
                   (fn [[x y z]] [z (- x) (- y)])
                   (fn [[x y z]] [z (- y) x])
                   (fn [[x y z]] [z x y])
                   (fn [[x y z]] [z y (- x)])]]
    (->> rotations
         (map (fn [rot]
                (->> probe
                     (map rot)))))))

(defn part1
  [input]
  (loop [beacons (first (all-beacons-as-origin (first input)))
         probes (rest input)]
    (if (empty? probes)
      (count beacons)
      (if-let [union (first (for [rotated-probe (all-orientations (first probes))
                                  probe (all-beacons-as-origin rotated-probe)
                                  beacons (all-beacons-as-origin beacons)
                                  :when (<= 12 (count (set/intersection (set probe) (set beacons))))]
                                  (set/union (set probe) (set beacons))))]
        (recur union (rest probes))
        (recur beacons (concat (rest probes) [(first probes)]))))))

(defn part2
  [input]
  (loop [beacons (first (all-beacons-as-origin (first input)))
         oprobes [(first input)]
         probes (rest input)]
    (if (empty? probes)
      (let [beacons (sort (remap beacons (first (sort beacons))))
            origin (first beacons)
            scanners (->> oprobes
                          (map (fn [probe]
                                 (let [from-scanner (first (sort probe))
                                       grounded (remap probe from-scanner)
                                       from-origin
                                       (first (for [p (all-beacons-as-origin beacons)
                                                    :when (set/subset? (set grounded) (set p))]
                                                (mapv - (first (sort p)))))]
                                   (v- from-origin from-scanner)))))]
        (->> (for [s1 scanners
                   s2 scanners]
               (->> (v- s1 s2)
                    (map (fn [^long x] (Math/abs x)))
                    (reduce +)))
             (reduce max)))
      (if-let [[union p] (first (for [rotated-probe (all-orientations (first probes))
                                      probe (all-beacons-as-origin rotated-probe)
                                      beacons (all-beacons-as-origin beacons)
                                      :when (<= 12 (count (set/intersection (set probe) (set beacons))))]
                                  [(set/union (set probe) (set beacons)) rotated-probe]))]
        (recur union (conj oprobes p) (rest probes))
        (recur beacons oprobes (concat (rest probes) [(first probes)]))))))
