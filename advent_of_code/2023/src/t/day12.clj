(ns t.day12
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [[symbols bounds] (s/split line #" ")]
                [symbols
                 (->> (re-seq #"\d+" bounds)
                      (map parse-long))])))))

(defn solve
  [in]
  (->> in
       (map (fn [[symbols pat]]
              (loop [to-process [[(s/split symbols #"\.") pat ""]]
                     matched []]
                (if (empty? to-process)
                  matched
                  (let [[s pat matched-so-far] (peek to-process), to-process (pop to-process)]
                    (prn [s pat matched-so-far to-process matched])
                    (cond (and (empty? pat) (->> s (every? (fn [g] (every? #{\?} g)))))
                          (recur to-process (conj matched (apply str matched-so-far (repeat (reduce + 0 (map count s)) \.))))
                          (or (empty? pat) (empty? s))
                          (recur to-process matched)
                          :else (let [g (first s), s (rest s)
                                      p (first pat), pat (rest pat)]
                                  (cond (and (zero? p) (= \? (first g)))
                                        (recur (conj to-process [(cons (subs g 1) s) pat (str matched-so-far \.)]) matched)
                                        (and (zero? p) (empty? g))
                                        (recur (conj to-process [s pat matched-so-far]) matched)
                                        (zero? p)
                                        (recur to-process matched)
                                        (and (> p (count g)) (every? #{\?} g))
                                        (recur (conj to-process [s (cons p pat) (apply str matched-so-far (repeat (count g) \.))]) matched)
                                        (> p (count g))
                                        (recur to-process matched)
                                        (empty? g)
                                        (recur to-process matched)
                                        (= \# (first g))
                                        (recur (conj to-process [(cons (subs g 1) s) (cons (dec p) pat) (str matched-so-far \#)]) matched)
                                        (= \? (first g))
                                        (recur (conj to-process
                                                     #_[(cons (subs g 1) s) (cons p pat) (str matched-so-far \.)]
                                                     [(cons (subs g 1) s) (cons (dec p) pat) (str matched-so-far \#)])
                                               matched)))))))))))

(defn part1
  [input]
  (->> input
       (drop 5)
       (take 1)
       solve))

(defn part2
  [input]
  (->> input
       (map (fn [[symbols pattern]]
              [(apply str (interpose \? (repeat 5 symbols)))
               (apply concat (repeat 5 pattern))]))
       solve))

(lib/check
  [part1 sample] 21
  #_#_[part1 puzzle] 7090
  #_#_[part2 sample] 525152
  #_#_[part2 puzzle] 0)
