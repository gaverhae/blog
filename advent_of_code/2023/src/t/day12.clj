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
                      (map parse-long)
                      vec)])))))

(defn match-line
  [[symbols pattern]]
  (loop [to-process [[(vec (re-seq #"[?#]+" symbols)) pattern]]
         matched 0]
    (if (empty? to-process)
      matched
      (let [[[symbols pattern] to-process] ((juxt peek pop) to-process)]
        (cond (and (empty? pattern) (or (empty? symbols)
                                        (every? (fn [s] (every? #{\?} s)) symbols)))
              (recur to-process (inc matched))
              (or (empty? pattern) (empty? symbols))
              (recur to-process matched)
              :else (let [[s symbols] ((juxt peek pop) symbols)
                          [p pattern] ((juxt peek pop) pattern)]
                      (cond (and (> p (count s)) (some #{\#} s))
                            (recur to-process matched)
                            (> p (count s))
                            (recur (conj to-process [symbols (conj pattern p)]) matched)
                            (= \# (get s p) (get s 0))
                            (recur to-process matched)
                            (= \? (get s 0))
                            (recur (conj to-process [(conj symbols (subs s 1)) (conj pattern p)]
                                                    [(conj symbols (str \# (subs s 1))) (conj pattern p)])
                                   matched)
                            (or (= p (count s))
                                (and (= (inc p) (count s)) (= \? (get s p))))
                            (recur (conj to-process [symbols pattern]) matched)
                            (= \? (get s p))
                            (recur (conj to-process [(conj symbols (subs s (inc p))) pattern]) matched))))))))

(defn part1
  [input]
  (->> input
       (map match-line)
       (reduce + 0)))

(defn part2
  [input]
  (->> input
       (map (fn [[symbols pattern]]
              (match-line [(str symbols "?" symbols "?" symbols "?" symbols "?" symbols)
                           (concat pattern pattern pattern pattern pattern)])))
       (reduce + 0)))

(lib/check
  [part1 sample] 21
  [part1 puzzle] 7090
  [part2 sample] 525152
  #_#_[part2 puzzle] 0)
