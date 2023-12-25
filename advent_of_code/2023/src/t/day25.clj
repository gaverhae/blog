(ns t.day25
  (:refer-clojure :exclude [rand-int])
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.data.int-map :as i]
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
              (s/split line #"[: ]+")))
       (map (fn [[c1 & conns]]
              (reduce (fn [acc el]
                        (-> acc
                            (update c1 (fnil conj #{}) el)
                            (update el (fnil conj #{}) c1)))
                      {}
                      conns)))
       (apply merge-with set/union)))

(defn part1
  [input]
  (->> input
       (filter (fn [[k vs]] (= 4 (count vs))))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 0
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
