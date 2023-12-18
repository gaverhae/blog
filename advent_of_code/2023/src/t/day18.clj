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
  (let [left [0 -1], right [0 1], up [-1 0], down [1 0]
        dug? (loop [to-dig (->> input
                                (mapcat (fn [[dir dist _]] (repeat dist dir))))
                    [y x] [0 0]
                    dug? {}]
               (if (empty? to-dig)
                 (assoc dug? [y x] up)
                 (let [[[dy dx] & to-dig] to-dig]
                   (recur to-dig
                          [(+ y dy) (+ x dx)]
                          (assoc dug? [y x] [dy dx])))))
        h (->> dug? keys (map first) (reduce max) inc)
        w (->> dug? keys (map second) (reduce max) inc)
        #_#_excavated (->> (range h)
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
    (->> (range h)
         (map (fn [y]
                (->> (range w)
                     (map (fn [x]
                            (condp = (dug? [y x])
                              nil \.
                              left \<
                              right \>
                              up \^
                              down \v)))
                     (apply str)
                     println)))
         doall)
    #_(count excavated)))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 62
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
