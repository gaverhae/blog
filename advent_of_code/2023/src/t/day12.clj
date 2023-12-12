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
  (let [process (fn [segment n]
                  (cond (and (> n (count segment)) (every? #{\?} segment)) [[false ""]]
                        (> n (count segment)) []
                        (= (first segment) \?) [[false (str \# (subs segment 1))]
                                                [false (subs segment 1)]]
                        (= n (count segment)) [[true ""]]
                        (= \? (get segment n)) [[true (subs segment (inc n))]]
                        (= \# (get segment n)) []
                        :else (throw (RuntimeException. (str "Unhandled: " (pr-str [segment n]))))))]
    (loop [to-process [[(re-seq #"[?#]+" symbols) pat]]
           n 0]
      (if (empty? to-process)
        n
        (let [[[[s & ss] [p & ps]] to-process] ((juxt peek pop) to-process)]
          (cond (and (nil? s) (nil? p)) (recur to-process (inc n))
                (and (nil? p) (every? (fn [segm] (every? #{\?} segm)) (cons s ss))) (recur to-process (inc n))
                (nil? p) (recur to-process n)
                (nil? s) (recur to-process n)
                :else (recur (reduce (fn [acc [drop? re]]
                                       (conj acc [(if (seq re) (cons re ss) ss)
                                                  (if drop? ps (cons p ps))]))
                                     to-process
                                     (process s p))
                             n)))))))

(defn part1
  [input]
  (->> input
       (map (fn [[s p]] (solve-line s p)))
       (reduce + 0)))

(defn part2
  [input]
  (->> input
       (map (fn [[s p]]
              (solve-line (str s \? s \? s \? s \? s)
                          (apply concat (repeat 5 p)))))
       (reduce + 0)))

(lib/check
  [part1 sample] 21
  [part1 puzzle] 7090
  [part2 sample] 525152
  #_#_[part2 puzzle] 0)
