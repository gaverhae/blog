(ns t.day12
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [[symbols bounds] (s/split line #" ")]
                [symbols
                 (->> (re-seq #"\d+" bounds)
                      (map parse-long))])))))

(defn solve-line
  [symbols pat]
  (loop [to-process [[(->> (s/split symbols #"\.")
                           (map (fn [segment]
                                  (->> segment
                                       (map {\? 0, \# 1})
                                       (into-array Long/TYPE)))))
                      pat]]
         matched 0]
    (if (empty? to-process)
      matched
      (let [[s pat] (peek to-process), to-process (pop to-process)]
        (cond (and (empty? pat) (->> s
                                     (every? (fn [^longs arr]
                                               (loop [i 0]
                                                 (cond (== i (alength arr)) true
                                                       (== 0 (aget arr i)) (recur (inc i))
                                                       (== 1 (aget arr i)) false))))))
              (recur to-process (inc matched))
              (or (empty? pat) (empty? s))
              (recur to-process matched)
              :else (let [^longs arr (first s), s (rest s)
                          p (first pat), pat (rest pat)]
                      (cond (and (> p (alength arr)) (loop [i 0]
                                                       (cond (== i (alength arr)) true
                                                             (== 0 (aget arr i)) (recur (inc i))
                                                             (== 1 (aget arr i)) false)))
                            (recur (conj to-process [s (cons p pat)]) matched)
                            (> p (alength arr))
                            (recur to-process matched)
                            (and (== 1 (aget arr 0))
                                 (> (alength arr) p)
                                 (== 1 (aget arr p)))
                            (recur to-process matched)
                            (== 0 (aget arr 0))
                            (recur (conj to-process
                                         [(cons (Arrays/copyOfRange arr 1 (alength arr)) s) (cons p pat)]
                                         [(cons (let [arc (Arrays/copyOf arr (alength arr))]
                                                  (aset arc 0 1)
                                                  arc)
                                                s)
                                          (cons p pat)])
                                   matched)
                            (or (== p (alength arr))
                                (and (== (inc p) (alength arr)) (== 0 (aget arr p))))
                            (recur (conj to-process [s pat]) matched)
                            (== 0 (aget arr p))
                            (recur (conj to-process [(cons (Arrays/copyOfRange arr (int (inc p)) (alength arr)) s) pat]) matched))))))))

(defn part1
  [input]
  (->> input
       (map (fn [[s p]] (solve-line s p)))
       (reduce + 0)))

(defn part2
  [input]
  (->> input
       (take 2)
       (map (fn [[s p]]
              (let [a (solve-line s p)
                    b (solve-line (str s "?" s) (apply concat (repeat 2 p)))
                    c (solve-line (str s "?" s "?" s) (apply concat (repeat 3 p)))
                    d (solve-line (str s "?" s "?" s "?" s) (apply concat (repeat 4 p)))
                    e (solve-line (str s "?") (apply concat (repeat 1 p)))
                    f (solve-line (str s "?" s "?") (apply concat (repeat 2 p)))
                    g (solve-line (str s "?" s "?" s "?") (apply concat (repeat 3 p)))
                    h (solve-line (str s "?" s "?" s "?" s "?") (apply concat (repeat 4 p)))
                    ]
                [a b c d e f g h])))
       #_(reduce + 0)))

(lib/check
  #_#_[part1 sample] 21
  #_#_[part1 puzzle] 7090
  [part2 sample] 525152
  [part2 puzzle] 0)
