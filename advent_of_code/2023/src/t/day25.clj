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

(defn connected?
  [links]
  (loop [todo [(->> links keys first)]
         found (set todo)]
    (if (empty? todo)
      (= found (->> links keys set))
      (let [[node & todo] todo
            nxt (->> (get links node)
                     (remove found))]
        (recur (reduce conj todo nxt)
               (conj found node))))))

(defn remove-link
  [links [from to]]
  (-> links
      (update from disj to)
      (update to disj from)))

(defn part1
  [input]
  [(connected? input)
   (connected? (-> input
                   (remove-link ["hfx" "pzl"])
                   (remove-link ["bvb" "cmg"])
                   (remove-link ["nvd" "jqt"])))])

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 0
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
