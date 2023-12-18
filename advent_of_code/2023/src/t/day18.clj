(ns t.day18
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
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
              (let [[dir dist color] (s/split line #" ")
                    dir (get {"R" [0 1], "D" [1 0], "L" [0 -1], "U" [-1 0]} dir)
                    dist (parse-long dist)]
                [dir dist color])))))

(defn part1
  [input]
  (let [dug? (loop [to-dig input
                    pos [0 0]
                    dug [[0 0]]]
               (if (empty? to-dig)
                 (set dug)
                 (let [[[dir dist] & to-dig] to-dig]
                   (recur to-dig
                          (mapv (fn [c d] (+ c (* d dist))) pos dir)
                          (reduce (fn [acc dist]
                                    (conj acc
                                          (mapv (fn [c d] (+ c (* d dist)))
                                                pos dir)))
                                  dug
                                  (range 1 (inc dist)))))))
        h (->> dug? (map first) (reduce max) inc)
        w (->> dug? (map second) (reduce max) inc)
        excavated (->> (range h)
                       (mapcat (fn [y]
                                 (->> (range w)
                                      (reduce (fn [[state dug] x]
                                                (match [state (dug? [y x])]
                                                  [:out nil] [:out dug]
                                                  [:out t] [:trench-in (conj dug [y x])]
                                                  [:trench-in nil] [:in (conj dug [y x])]
                                                  [:trench-in t] [:trench-in (conj dug [y x])]
                                                  [:in nil] [:in (conj dug [y x])]
                                                  [:in t] [:trench-out (conj dug [y x])]
                                                  [:trench-out nil] [:out dug]
                                                  [:trench-out t] [:trench-out (conj dug [y x])]))
                                              [:out []])
                                      second))))]
    (count excavated)))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 62
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
