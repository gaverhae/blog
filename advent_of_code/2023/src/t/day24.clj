(ns t.day24
  (:refer-clojure :exclude [rand-int])
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.data.int-map :as i]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [[_ x y z dx dy dz] (re-find #" *(-?\d+), +(-?\d+), +(-?\d+) +@ +(-?\d+), +(-?\d+), +(-?\d+)" line)]
                (->> (mapv (comp bigint parse-long) [x y z dx dy dz])
                     ((fn [[x y z dx dy dz]]
                        [[x y z] [dx dy dz]]))))))))

(defn line-intersection
  [l1 l2]
  ;; DOES NOT COVER ALL CASES
  ;; Input never has 0 as a direction (=> (not= c1 0))
  (let [[[x1 x2] [c1 c2]] l1
        [[y1 y2] [d1 d2]] l2]
    (prn [:d (* d1 c2) (* d2 c1)])
    (when-not (= (* d1 c2) (* d2 c1))
      (let [b (/ (+ (* y2 c1) (* -1 x2 c1) (* -1 y1 c2) (* x1 c2))
                 (+ (* d1 c2) (* -1 d2 c1)))
            a (/ (+ y1 (* b d1) (- x1))
                 c1)]
        (prn [:c (->> (concat l1 l2)
                      lib/transpose
                      (map (fn [[xn cn yn dn]]
                             [(+ xn (* a cn)) (+ yn (* b dn))])))])
        (when (->> (concat l1 l2)
                   lib/transpose
                   (every? (fn [[xn cn yn dn]]
                             (= (+ xn (* a cn)) (+ yn (* b dn))))))
          [a b (->> l1 lib/transpose (map (fn [[xn cn]] (+ xn (* a cn)))))])))))

(defn part1
  [input min-c max-c]
  (let [input (->> input
                   (map (fn [[[x y z] [dx dy dz]]]
                          [[x y] [dx dy]])))]
    (->> (map vector input (iterate rest (rest input)))
         (mapcat (fn [[l1 ls]]
                   (->> ls
                        (map (fn [l2] [l1 l2])))))
         (keep (fn [[l1 l2]]
                (line-intersection l1 l2)))
         (filter (fn [[a b [x y]]]
                   (and (>= a 0)
                        (>= b 0)
                        (<= min-c x max-c)
                        (<= min-c y max-c))))
         count)))

(defn cross-product
  [[a1 a2 a3] [b1 b2 b3]]
  [(- (* a2 b3) (* a3 b2))
   (- (* a3 b1) (* a1 b3))
   (- (* a1 b2) (* a2 b1))])

(defn dot-product
  [[a1 a2 a3] [b1 b2 b3]]
  (+ (* a1 b1) (* a2 b2) (* a3 b3)))

(defn vector-minus
  [[a1 a2 a3] [b1 b2 b3]]
  [(- a1 b1) (- a2 b2) (- a3 b3)])

(defn vector-plus
  [[a1 a2 a3] [b1 b2 b3]]
  [(+ a1 b1) (+ a2 b2) (+ a3 b3)])

(defn scalar-mult
  [scalar [a1 a2 a3]]
  [(* scalar a1) (* scalar a2) (* scalar a3)])

(defn plane-from-three-points
  [p1 p2 p3]
  (let [n (cross-product (vector-minus p2 p1)
                         (vector-minus p3 p1))]
    [p1 n]))

(defn plane-line-intersection
  [line plane]
  (let [[l0 l] line
        [p0 n] plane
        denom (dot-product l n)
        nom (dot-product (vector-minus p0 l0)
                         n)]
    (if (zero? denom)
      (if (zero? nom)
        [:line line]
        [:none])
      [:point (vector-plus l0 (scalar-mult (/ nom denom) l))])))

(defn line-from-two-points
  [[a1 a2 a3] [b1 b2 b3]]
  [[a1 a2 a3] [(- b1 a1) (- b2 a2) (- b3 a3)]])

(defn is-point-on-line?
  [[[a1 a2 a3] [d1 d2 d3]] [p1 p2 p3]]
  (let [c (/ (- p1 a1) d1)]
    (and (= (+ a1 (* c d1)) p1)
         (= (+ a2 (* c d2)) p2)
         (= (+ a3 (* c d3)) p3))))

(defn vector-length
  [[x y z]]
  (Math/sqrt (+ (* 1.0 x x) (* 1.0 y y) (* 1.0 z z))))

(defn gen-search
  [lines seed]
  (let [rng (java.util.Random. seed)
        rand-int (fn [m] (bigint (* m (.nextDouble rng))))
        carousel (fn [p] (let [maxi (reduce max (map first p))
                               inverted (map (fn [[f i]] [(- maxi f) f i]) p)
                               total (reduce + (map first inverted))
                               roll (rand-int total)]
                           (loop [r roll
                                  [[f' f s] & p] inverted]
                             (if (<= r f')
                               [f s]
                               (recur (- r f') p)))))
        num-lines (count lines)
        max-time (bigint Long/MAX_VALUE)
        make-solution (fn []
                        (vec (repeatedly num-lines #(rand-int max-time))))
        fitness (fn [ts]
                  (->> (map vector lines ts)
                       (map (fn [[[[x y z] [dx dy dz]] t]]
                              [t
                               (+ x (* t dx))
                               (+ y (* t dy))
                               (+ z (* y dz))]))
                       sort
                       (partition 2 1)
                       (map (fn [[[_ x1 y1 z1] [x2 y2 z2]]]
                              (let [dx (- x1 x2), dy (- y1 y2), dz (- z1 z2)]
                                (+ (* dx dx) (* dy dy) (* dz dz)))))
                       (reduce + 0)))
        mutate (fn [ts]
                 (assoc ts (rand-int num-lines) (rand-int max-time)))
        crossover (fn [t1 t2]
                    (let [cut (rand-int num-lines)]
                      (vec (concat (take cut t1)
                                   (drop cut t2)))))
        init-pop (->> (repeatedly 100 make-solution)
                      (map (fn [i] [(fitness i) i]))
                      sort)
        start-time (lib/now-millis)]
    (loop [population init-pop
           step 0]
      (when (zero? (rem step 1000))
        (prn [(lib/duration-since start-time) (ffirst population)]))
      (if (zero? (ffirst population))
        (second (first population))
        (recur (let [survivors (concat (take 10 population)
                                       (take 3 (reverse population)))
                     new-spawns (->> (repeatedly 10 make-solution)
                                     (map (fn [i] [(fitness i) i])))
                     children (repeatedly
                                77
                                #(let [[_ parent1] (carousel population)
                                       [_ parent2] (carousel population)
                                       child (mutate (crossover parent1 parent2))]
                                   [(fitness child) child]))]
                 (sort (concat survivors new-spawns children)))
               (inc step))))))

(defn part2
  [input]
  (gen-search input 0))

(lib/check
  #_#_[part1 sample 7 27] 2
  #_#_[part1 puzzle 200000000000000 400000000000000] 20336
  #_#_[part2 sample] 47
  [part2 puzzle] 0)
