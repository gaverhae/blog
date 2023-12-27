(ns t.day24
  (:refer-clojure :exclude [rand-int])
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.data.int-map :as i]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [numeric.expresso.core :as ex]
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

(defn part2
  [input]
  (let [[[[a1 a2 a3] [da1 da2 da3]]
         [[b1 b2 b3] [db1 db2 db3]]
         [[c1 c2 c3] [dc1 dc2 dc3]]] input]
    (ex/solve '[x1 x2 x3 dx1 dx2 dx3 t1 t2 t3]
              (ex/ex' [x1 x2 x3 dx1 dx2 dx3 t1 t2 t3] (= (+ x1 (* t1 dx1)) (+ a1 (* t1 da1))))
              (ex/ex' [x1 x2 x3 dx1 dx2 dx3 t1 t2 t3] (= (+ x2 (* t1 dx2)) (+ a2 (* t1 da2))))
              (ex/ex' [x1 x2 x3 dx1 dx2 dx3 t1 t2 t3] (= (+ x3 (* t1 dx3)) (+ a3 (* t1 da3))))
              (ex/ex' [x1 x2 x3 dx1 dx2 dx3 t1 t2 t3] (= (+ x1 (* t2 dx1)) (+ b1 (* t2 db1))))
              (ex/ex' [x1 x2 x3 dx1 dx2 dx3 t1 t2 t3] (= (+ x2 (* t2 dx2)) (+ b2 (* t2 db2))))
              (ex/ex' [x1 x2 x3 dx1 dx2 dx3 t1 t2 t3] (= (+ x3 (* t2 dx3)) (+ b3 (* t2 db3))))
              (ex/ex' [x1 x2 x3 dx1 dx2 dx3 t1 t2 t3] (= (+ x1 (* t3 dx1)) (+ c1 (* t3 dc1))))
              (ex/ex' [x1 x2 x3 dx1 dx2 dx3 t1 t2 t3] (= (+ x2 (* t3 dx2)) (+ c2 (* t3 dc2))))
              (ex/ex' [x1 x2 x3 dx1 dx2 dx3 t1 t2 t3] (= (+ x3 (* t3 dx3)) (+ c3 (* t3 dc3)))))))

(lib/check
  [part1 sample 7 27] 2
  [part1 puzzle 200000000000000 400000000000000] 20336
  [part2 sample] 47
  #_#_[part2 puzzle] 0)
