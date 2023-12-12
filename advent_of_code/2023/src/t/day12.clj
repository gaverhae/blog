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
                  (count matched)
                  (let [[s pat matched-so-far] (peek to-process), to-process (pop to-process)]
                    (cond (and (empty? pat) (->> s (every? (fn [g] (every? #{\?} g)))))
                          (recur to-process (conj matched (apply str matched-so-far (repeat (reduce + 0 (map count s)) \.))))
                          (or (empty? pat) (empty? s))
                          (recur to-process matched)
                          :else (let [g (first s), s (rest s)
                                      p (first pat), pat (rest pat)]
                                  (cond (and (> p (count g)) (every? #{\?} g))
                                        (recur (conj to-process [s (cons p pat) (apply str (concat matched-so-far (repeat (count g) \.)))]) matched)
                                        (> p (count g))
                                        (recur to-process matched)
                                        (= \# (get g p) (first g))
                                        (recur to-process matched)
                                        (= \? (first g))
                                        (recur (conj to-process
                                                     [(cons (subs g 1) s) (cons p pat) (str matched-so-far \.)]
                                                     [(cons (str \# (subs g 1)) s) (cons p pat) matched-so-far])
                                               matched)
                                        (or (= p (count g))
                                            (and (= (inc p) (count g)) (= \? (get g p))))
                                        (recur (conj to-process [s pat (apply str (concat matched-so-far (repeat p \#) (when (= (count g) (inc p)) [\.])))]) matched)
                                        (= \? (get g p))
                                        (recur (conj to-process
                                                     [(cons (subs g (inc p)) s) pat (apply str (concat matched-so-far (repeat p \#) [\.]))])
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
