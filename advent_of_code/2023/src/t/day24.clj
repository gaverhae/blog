(ns t.day24
  (:refer-clojure :exclude [rand-int])
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as ml]
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
                (->> (mapv parse-long [x y z dx dy dz])
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

(defn vector-minus
  [v1 v2]
  (mapv (fn [a b] (- a b)) v1 v2))

(defn solve
  ;; https://www.baeldung.com/cs/solving-system-linear-equations
  [a b]
  (let [a (loop [j 0
                 [a b] [a b]]
            (prn [:loop a b])
            (if (= j (count a))
              a
              (recur (inc j)
                     (let [[a b] (if (zero? (get-in a [j j]))
                                   (let [[big, krow]
                                         (loop [big 0
                                                krow j
                                                k (inc j)]
                                           (if (= k (count a))
                                             [big krow]
                                             (if (> (abs (get-in a [k j])) big)
                                               (recur (long (abs (get-in a [k j]))) k (inc k))
                                               (recur big krow (inc k)))))
                                         a (loop [l j, a a]
                                             (if (= l (count a))
                                               a
                                               (recur (inc l)
                                                      (-> a
                                                          (assoc-in [j l] (get-in a [krow l]))
                                                          (assoc-in [krow l] (get-in a [j l]))))))
                                         b (-> b
                                               (assoc j (get b krow))
                                               (assoc krow (get b j)))]
                                     [a b])
                                   [a b])
                           pivot (get-in a [j j])]
                       (if (zero? pivot)
                         (throw (Exception. "Singular matrix."))
                         (loop [i (inc j)
                                [a b] [a b]]
                           (if (= (count a) i)
                             [a b]
                             (let [mult (/ (get-in a [i j]) pivot)
                                   a (loop [l j, a a]
                                       (if (= l (count a))
                                         a
                                         (recur (inc l)
                                                (assoc-in a [i l] (- (get-in a [i l]) (* mult (get-in a [j l])))))))
                                   b (assoc b i (- (get b i) (* mult (get b j))))]
                               (recur (inc i) [a b])))))))))]
    (loop [i (dec (count a))
           x (vec (repeat (count b) nil))]
      (if (= -1 i)
        [a b x]
        (recur (dec i)
               (let [sum (loop [j (inc i), sum 0]
                           (if (= (count a) j)
                             sum
                             (recur (inc j)
                                    (+ sum (* (get x j) (get-in a [i j]))))))
                     x (assoc x i (* (/ 1 (get-in a [i i])) (- (get b i) sum)))]
                 x))))))

(defn part2
  [input]
  (let [[[x1 v1] [x2 v2] [x3 v3]] input
        ;; for each i: x0 + (ti * v0) = xi + (ti * vi)
        ;; <=> (x0 - xi) = -ti * (v0 - vi)
        ;; ==> (x0 - xi) and (v0 - vi) are parallel ==> (x0 - xi) * (v0 - vi) = 0
        ;; (where * is cross product, x0/v0 is the stone and xi/ti are the hails)
        ;;
        ;; This can expand to (cross product is distributive over addition):
        ;; (x0 * v0) - (x0 * vi) - (xi * v0) + (xi * vi) = 0
        ;; which is not linear but only in the common term (x0 * v0), which we
        ;; can equate between hails.
        ;; Taking the first three hails, we get:
        ;; (x1 * v1) - (x0 * v1) - (x1 * v0) = (x2 * v2) - (x0 * v2) - (x2 * v0)
        ;; (x1 * v1) - (x0 * v1) - (x1 * v0) = (x3 * v3) - (x0 * v3) - (x3 * v0)
        ;; which is a linear system of 6 equations with 6 unknowns.
        ;; Say we want AX = B, then B is given by (x1 * v1) - (x2 * v2) for its
        ;; first three rows, then (x1 * v1) - (x3 * v3) for the last three.
        x1v1 (cross-product x1 v1), x2v2 (cross-product x2 v2), x3v3 (cross-product x3 v3)
        B (vec (concat (vector-minus x1v1 x2v2)
                       (vector-minus x1v1 x3v3)))
        ;; and A is given by (v2 - v1) * x0 + (x1 - x2) * v0 and idem for 3.
        ;; (Note that it's v2-v1 and not v1-v2 because the cross product is
        ;; anti-commutative.)
        ;; We don't have x0 and v0 to make the cross-product, but if we do it
        ;; algebraically we get:
        ;; [a, b, c] * [x0, x1, x2] => [bx2 - cx1, cx0 - ax2, ax1 - bx0] which,
        ;; in terms of a 3x3 square in the A matrix, yields:
        ;; [[0, -c, b], [c, 0, -a], [-b, a, 0]] (where [a b c] is e.g. v1 - v2)
        ;; so:
        make-square (fn [[a b c]] [[0 (- c) b] [c 0 (- a)] [(- b) a 0]])
        a11 (make-square (vector-minus v2 v1))
        a12 (make-square (vector-minus x1 x2))
        a21 (make-square (vector-minus v3 v1))
        a22 (make-square (vector-minus x1 x3))
        ;; can't think of a clever way to do this right now
        A [(vec (concat (get a11 0) (get a12 0)))
           (vec (concat (get a11 1) (get a12 1)))
           (vec (concat (get a11 2) (get a12 2)))
           (vec (concat (get a21 0) (get a22 0)))
           (vec (concat (get a21 1) (get a22 1)))
           (vec (concat (get a21 2) (get a22 2)))]]
    (m/set-current-implementation :vectorz)
    #_(prn (ml/solve (m/matrix A) (m/matrix B)))
    #_(prn B)
    (prn [:solve (solve A B)])
    #_(->> (ml/solve (m/matrix A) (m/matrix B))
         (take 3)
         (reduce + 0)
         )))

(lib/check
  #_#_[part1 sample 7 27] 2
  #_#_[part1 puzzle 200000000000000 400000000000000] 20336
  [part2 sample] 47
  #_#_[part2 puzzle] 677656046662768)
