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
  (let [dug? (loop [to-dig (->> input
                                (mapcat (fn [[dir dist _]] (repeat dist dir))))
                    [y x] [0 0]
                    dug? {}]
               (if (empty? to-dig)
                 (assoc dug? [y x] [-1 0])
                 (let [[[dy dx] & to-dig] to-dig]
                   (recur to-dig
                          [(+ y dy) (+ x dx)]
                          (assoc dug? [y x] [dy dx])))))
        min-y (->> dug? keys (map first) (reduce min))
        max-y (->> dug? keys (map first) (reduce max) inc)
        min-x (->> dug? keys (map second) (reduce min))
        max-x (->> dug? keys (map second) (reduce max) inc)
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
    (->> (range min-y max-y)
         (map (fn [y]
                (->> (range min-x max-x)
                     (map (fn [x]
                            (condp = (dug? [y x])
                              nil \space
                              [0 -1] \<
                              [0 1] \>
                              [-1 0] \^
                              [1 0] \v)))
                     (apply str)
                     println)))
         doall)
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
