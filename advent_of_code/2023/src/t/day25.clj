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
  (let [a (->> graph keys first)
        step (fn [graph weights]
               (let [V (->> graph keys set)]
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
        merge-vertices (fn [graph weights merged s t]
                         (let [from-t (->> (get graph t)
                                           (remove #{s}))
                               t-weights (->> from-t
                                              (map (fn [v] [v (get weights #{v t})])))
                               t-size (get merged t)]
                           [(reduce (fn [g v]
                                      (-> g
                                          (update v disj t)
                                          (update v conj s)))
                                    (-> graph
                                        (dissoc t)
                                        (update s disj t)
                                        (update s set/union (set from-t)))
                                    from-t)
                            (reduce (fn [ws [v w]]
                                      (-> ws
                                          (dissoc #{t v})
                                          (update #{s v} (fnil + 0) w)))
                                    (-> weights
                                        (dissoc #{s t}))
                                    t-weights)
                            (-> merged
                                (dissoc t)
                                (update s + t-size))]))
        start-time (lib/now-millis)]
    (loop [[graph weights merged] [graph
                                   (->> graph
                                        (mapcat (fn [[k vs]]
                                                  (->> vs
                                                       (map (fn [v] #{k v})))))
                                        set
                                        (map (fn [e] [e 1]))
                                        (into {}))
                                   (->> graph keys (map (fn [k] [k 1])) (into {}))]
           [best-w part-size] [Long/MAX_VALUE 0]]
      (if (= 1 (count graph))
        part-size
        (let [[s t w] (step graph weights)
              new-part (conj part t)]
          (prn [(lib/now) (lib/duration-since start-time)
                :graph (count graph)
                :weights (->> weights vals (reduce + 0))
                :s-t (get weights #{s t})
                :cut [s t w]
                :best best-w part-size])
          (recur (merge-vertices graph weights merged s t)
                 (if (< w best-w)
                   [w (get merged t)]
                   [best-w part-size])))))))

(defn part1
  [input]
  (let [c (stoer-wagner input)]
    (* c (- (count input) c))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 54
  [part1 puzzle] 527790
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
