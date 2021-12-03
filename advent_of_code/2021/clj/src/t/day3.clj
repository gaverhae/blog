(ns t.day3
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]))

(defn parse
  [lines]
  lines)

(defn part1
  [input]
  (let [gamma (->> input
                   (apply mapv vector)
                   (map frequencies)
                   (map (fn [s] (->> s (sort-by val) last key))))
        epsilon (->> gamma
                     (map {\0 \1, \1 \0}))]
    (->> [gamma epsilon]
         (map (fn [s] (apply str s)))
         (map (fn [s] (Long/parseLong s 2)))
         (reduce * 1))))

(defn part2
  [input]
  (let [len (->> input first count)
        most-common (fn [s]
                      (->> s
                           (map first)
                           frequencies
                           ((fn [f]
                              (if (= (f \0) (f \1))
                                \1
                                (->> f (sort-by val) last key))))))
        least-common (fn [s]
                       (let [m (most-common s)]
                         ({\1 \0, \0 \1} m)))
        rating (fn [f]
                 (loop [r input
                        p 0
                        s ""]
                   (cond (= p len)
                         (Long/parseLong s 2)
                         (= 1 (count r))
                         (Long/parseLong (apply str s (first r)) 2)
                         :else
                         (let [next-bit (f r)]
                           (recur (->> r
                                       (filter (fn [r] (= next-bit (first r))))
                                       (map rest))
                                  (inc p)
                                  (str s next-bit))))))]

    (* (rating most-common)
     (rating least-common))))

