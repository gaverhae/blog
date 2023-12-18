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
                    [py px] [0 1]
                    dug? {[0 0] \F}]
               (if (empty? to-dig)
                 (assoc dug? [y x] \F)
                 (let [[[dy dx] & to-dig] to-dig]
                   (recur to-dig
                          [(+ y dy) (+ x dx)]
                          [y x]
                          (assoc dug? [y x] (match [(- y py) (- x px) dy dx]
                                              [ 0  1  1  0] \7
                                              [-1  0  0 -1] \7
                                              [-1  0  0  1] \F
                                              [ 0 -1  1  0] \F
                                              [ _  0  _  0] \|
                                              [ 0  _  0  _] \-
                                              [ 1  0  0  1] \L
                                              [ 0 -1 -1  0] \L
                                              [ 0  1 -1  0] \J
                                              [ 1  0  0 -1] \J))))))
        min-y (->> dug? keys (map first) (reduce min))
        max-y (->> dug? keys (map first) (reduce max) inc)
        min-x (->> dug? keys (map second) (reduce min))
        max-x (->> dug? keys (map second) (reduce max) inc)
        drawing (->> (range min-y max-y)
                     (map (fn [y]
                            (->> (range min-x max-x)
                                 (map (fn [x] (dug? [y x] \space)))
                                 (apply str)))))
        excavated (->> (range min-y max-y)
                       (mapcat (fn [y]
                                 (->> (range min-x max-x)
                                      (reduce (fn [[state dug] x]
                                                (match [state (dug? [y x])]
                                                  [[:out] nil] [[:out] dug]
                                                  [[:out] dir] [[:trench-in dir] (conj dug [y x])]
                                                  [[:trench-in _] nil] [[:in] (conj dug [y x])]
                                                  [[:trench-in [-1 0]] [-1 0]] [[:in] (conj dug [y x])]
                                                  [[:trench-in [-1 0]] [1 0]] [[:out] (conj dug [y x])]
                                                  [[:trench-in [1 0]] [1 0]] [[:in] (conj dug [y x])]
                                                  [[:trench-in [1 0]] [-1 0]] [[:out] (conj dug [y x])]
                                                  [[:trench-in dir] _] [[:trench-in dir] (conj dug [y x])]
                                                  [[:in] nil] [[:in] (conj dug [y x])]
                                                  [[:in] dir] [[:trench-out dir] (conj dug [y x])]
                                                  [[:trench-out _] nil] [[:out] dug]
                                                  [[:trench-out [-1 0]] [-1 0]] [[:out] (conj dug [y x])]
                                                  [[:trench-out [-1 0]] [1 0]] [[:in] (conj dug [y x])]
                                                  [[:trench-out [1 0]] [-1 0]] [[:in] (conj dug [y x])]
                                                  [[:trench-out [1 0]] [1 0]] [[:out] (conj dug [y x])]
                                                  [[:trench-out dir] _] [[:trench-out dir] (conj dug [y x])]))
                                              [[:out] []])
                                      second)))
                       set)]
    (->> drawing (map println) doall)
    (->> (range min-y max-y)
         (map (fn [y]
                (->> (range min-x max-x)
                     (map (fn [x]
                            (if (excavated [y x])
                              \# \space)))
                     (apply str)
                     println)))
         doall)
    (count excavated)))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 62
  [part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
