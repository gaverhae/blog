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
  ([probe] (remap probe (first (sort probe))))
  ([probe v0]
   (->> probe (mapv #(v- % v0)))))

(defn encode
  [[x y z]]
  (+ (+ 5000 x)
     (* 10000 (+ 5000 y))
     (* 10000 10000 (+ 5000 z))))

(defn inter
  [p1 p2]
  (count (set/intersection p1 p2)))

(defn all-beacons-as-origin
  [probe]
  (->> probe
       (map (fn [v] (set (remap probe v))))
       (map (fn [beacons] [beacons (set (map encode beacons))]))))


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

(defn unify-beacons
  [input end-fn]
  (loop [beacons (remap (first input))
         bremp (all-beacons-as-origin beacons)
         oprobes [(first input)]
         probes (->> (rest input)
                     (map (fn [probe]
                            (->> (all-orientations probe)
                                 (map (fn [p]
                                        [p (all-beacons-as-origin p)]))))))]
    (if (empty? probes)
      (end-fn beacons oprobes)
      (if-let [[union p] (first (for [[beacons benc] bremp
                                      [rotated-probe remapped-probes] (first probes)
                                      [probe penc] remapped-probes
                                      :when (<= 12 (inter penc benc))]
                                  [(set/union probe beacons) rotated-probe]))]
        (recur union (all-beacons-as-origin union)
               (conj oprobes p)
               (rest probes))
        (recur beacons bremp oprobes
               (concat (rest probes) [(first probes)]))))))

(defn part1
  [input]
  (unify-beacons input (fn [beacons _] (count beacons))))

(defn part2
  [input]
  (unify-beacons
    input
    (fn [beacons oprobes]
      (let [scanners (->> oprobes
                          (map (fn [probe]
                                 (let [from-scanner (first (sort probe))
                                       grounded (remap probe from-scanner)
                                       from-origin
                                       (first (for [[p _] (all-beacons-as-origin beacons)
                                                    :when (set/subset? (set grounded) (set p))]
                                                (mapv - (first (sort p)))))]
                                   (v- from-origin from-scanner)))))]
        (->> (for [s1 scanners
                   s2 scanners]
               (->> (v- s1 s2)
                    (map (fn [^long x] (Math/abs x)))
                    (reduce +)))
             (reduce max))))))
