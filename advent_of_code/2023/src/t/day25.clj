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
  (let [links (->> input
                  (mapcat (fn [[k vs]]
                            (->> vs (map (fn [v] (-> [k v] sort vec))))))
                  set
                  sort
                  vec)]
    (for [idx1 (range (count links))
          idx2 (range idx1 (count links))
          idx3 (range idx2 (count links))
          :when (not (connected? (-> input
                                     (remove-link (get links idx1))
                                     (remove-link (get links idx2))
                                     (remove-link (get links idx3)))))]
      (->> [idx1 idx2 idx3] (map (fn [idx] (get links idx)))))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 0
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
