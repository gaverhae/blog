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

;; https://dl.acm.org/doi/10.1145/263867.263872
(defn stoer-wagner
  [graph]
  (let [step (fn [graph weights]
               (let [V (->> graph keys set)
                     a (first V)]
                 (loop [A #{a}
                        ordered-A (list a)
                        candidates (disj V a)
                        last-weight 0]
                   (if (empty? candidates)
                     [(second ordered-A) (first ordered-A) last-weight]
                     (let [[w c] (->> candidates
                                      (map (fn [c]
                                             [(->> (get graph c)
                                                   (filter A)
                                                   (map (fn [a]
                                                          (get weights #{a c})))
                                                   (reduce + 0))
                                              c]))
                                      sort
                                      last)]
                       (recur (conj A c)
                              (conj ordered-A c)
                              (disj candidates c)
                              (long w)))))))
        merge-vertices (fn [graph weights s t]
                         (let [common (set/intersection (graph s) (graph t))]
                           [(reduce (fn [acc el]
                                      (-> acc
                                          (update s conj el)
                                          (update el conj s)))
                                    (->> graph
                                         (remove (fn [[k vs]] (= k t)))
                                         (map (fn [[k vs]] [k (disj vs t)]))
                                         (into {}))
                                    (->> (graph t)
                                         (remove #{s})))
                            (reduce (fn [acc el]
                                      (update acc #{el s} (fnil + 0) (get weights #{el t})))
                                    (->> weights
                                         (remove (fn [[k v]] (get k t)))
                                         (into {}))
                                    (->> (graph t)
                                         (remove #{s})))]))]
    (loop [[graph weights] [graph (->> graph
                                       (mapcat (fn [[k vs]]
                                                 (->> vs
                                                      (map (fn [v] #{k v})))))
                                       set
                                       (map (fn [e] [e 1]))
                                       (into {}))]
           [best-w best-part] [(count graph) #{}]
           part #{}]
      (if (= 1 (count graph))
        best-part
        (let [[s t w] (step graph weights)
              new-part (conj part t)]
          (recur (merge-vertices graph weights s t)
                 (if (< w best-w)
                   [w new-part]
                   [best-w best-part])
                 new-part))))))

(defn part1
  [input]
  (let [part (stoer-wagner input)
        c (count part)]
    (* c (- (count input) c))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 54
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
