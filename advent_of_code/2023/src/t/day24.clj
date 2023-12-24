(ns t.day24
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
    (when-not (= (* d1 c2) (* d2 c1))
      (let [b (/ (+ (* y2 c1) (* -1 x2 c1) (* -1 y1 c2) (* x1 c2))
                 (+ (* d1 c2) (* -1 d2 c1)))
            a (/ (+ y1 (* b d1) (- x1))
                 c1)]
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

(defn part2
  [input]
  (let [plane (->> ;; we start by taking all pairs of lines
                   (map vector input (iterate rest (rest input)))
                   (mapcat (fn [[l1 ls]]
                             (->> ls
                                  (mapcat (fn [l2] [[l1 l2] [l2 l1]])))))
                   ;; then, for each pair, we make a plane by assuming (for
                   ;; now) that the stone hits the first of the two hails at
                   ;; t = 0; we do not know when the stone will cut through
                   ;; the second hail so we form a plane with the first hail
                   ;; at t=0 (so just its position) and the second hail's
                   ;; trajectory (any two points)
                   (map (fn [[[x _] [y d]]]
                          (plane-from-three-points x y (vector-plus y d))))
                   ;; we keep the planes that intersect all the lines
                   (filter (fn [plane]
                             (->> input
                                  (every? (fn [line] (not= [:none] (plane-line-intersection line plane)))))))
                   ;; for each plane, we keep all the intersections
                   (map (fn [plane]
                          (->> input (map (fn [line] (plane-line-intersection line plane))))))
                   ;; for a single candidate plane, all the points must form a line
                   (filter (fn [inters]
                             (let [points (->> inters (filter (fn [[t _]] (= t :point))) (map second))]
                               (if (= 2 (count points))
                                 ;; two points always form a line
                                 true
                                 (let [[p1 p2 & ps] points
                                       line (line-from-two-points p1 p2)]
                                   (->> ps (every? #(is-point-on-line? line %))))))))
                   ;; we replace all the points with a single line
                   (map (fn [inters]
                          (->> inters
                               (remove (fn [[t _]] (= t :line)))
                               (map (fn [[_ p]] p))
                               (take 2)
                               (apply line-from-two-points))))
                   ;; at this point we have the trajectory of the stone, but we
                   ;; need to know its starting position; first, we compute the
                   ;; intersection with each hail
                   (map (fn [stone]
                          [stone
                           (->> input
                                (map (fn [hail] (line-intersection hail stone)))
                                ;; we care about timings, we don't care about
                                ;; where intersections happen
                                (map (fn [[a b _]] [a b])))]))
                   ;; if things work out as expected, each [a b] tuple should
                   ;; have the same (- b a) value, which is the skew between
                   ;; the hail timeline and the stoen timeline (i.e. the
                   ;; unknown offset we introduced when we decided to set t = 0
                   ;; on the first step above)
                   ;; with the adjusted t = 0 we get the initial position of the stone
                   (map (fn [[stone times]]
                          (if (->> times
                                   (map (fn [[a b]] (- b a)))
                                   (apply =))
                            (let [[a b] (first times)
                                  offset (- b a)
                                  [p d] stone]
                              (vector-plus p (scalar-mult offset d)))
                            (throw (Exception. "Unexpected condition")))))
                   (map (fn [[x y z]] (+ x y z))))]
    [(count plane) plane]))

(lib/check
  [part1 sample 7 27] 2
  [part1 puzzle 200000000000000 400000000000000] 20336
  [part2 sample] 47
  [part2 puzzle] 0)
