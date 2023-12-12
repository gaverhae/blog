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
              (loop [to-process [[(s/split symbols #"\.") pat]]
                     matched 0]
                (if (empty? to-process)
                  matched
                  (let [[s pat] (peek to-process), to-process (pop to-process)]
                    (cond (and (empty? pat) (->> s (every? (fn [g] (every? #{\?} g)))))
                          (recur to-process (inc matched))
                          (or (empty? pat) (empty? s))
                          (recur to-process matched)
                          :else (let [g (first s), s (rest s)
                                      p (first pat), pat (rest pat)]
                                  (cond (and (> p (count g)) (every? #{\?} g))
                                        (recur (conj to-process [s (cons p pat)]) matched)
                                        (> p (count g))
                                        (recur to-process matched)
                                        (= \# (get g p) (first g))
                                        (recur to-process matched)
                                        (= \? (first g))
                                        (recur (conj to-process
                                                     [(cons (subs g 1) s) (cons p pat)]
                                                     [(cons (str \# (subs g 1)) s) (cons p pat)])
                                               matched)
                                        (or (= p (count g))
                                            (and (= (inc p) (count g)) (= \? (get g p))))
                                        (recur (conj to-process [s pat]) matched)
                                        (= \? (get g p))
                                        (recur (conj to-process
                                                     [(cons (subs g (inc p)) s) pat])
                                               matched)))))))))
       #_(map-indexed (fn [i x] (prn [(inc i) x]) x))
       (reduce + 0)))

(defn part1
  [input]
  (->> input
       solve))

(defn unchunk
  [s]
  (when (seq s)
    (lazy-seq
      (cons (first s)
            (unchunk (next s))))))

(defn part2
  [input]
  (->> input
       (map (fn [[symbols pattern]]
              [(apply str (interpose \? (repeat 5 symbols)))
               (apply concat (repeat 5 pattern))]))
       unchunk
       solve))

(lib/check
  [part1 sample] 21
  [part1 puzzle] 7090
  [part2 sample] 525152
  #_#_[part2 puzzle] 0)
