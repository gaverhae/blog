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
                         (let [from-t (->> (get graph t)
                                           (remove #{s}))
                               t-weights (->> from-t
                                              (map (fn [v] [v (get weights #{v t})])))]
                           [(reduce (fn [g v]
                                      (-> g
                                          (update v disj t)
                                          (update v conj s)))
                                    (-> graph
                                        (dissoc t)
                                        (update s disj t)
                                        (update s set/union from-t))
                                    from-t)
                            (reduce (fn [ws [v w]]
                                      (-> ws
                                          (dissoc #{t v})
                                          (update #{s v} (fnil + 0) w)))
                                    (-> weights
                                        (dissoc #{s t}))
                                    t-weights)]))
        start-time (lib/now-millis)]
    (loop [[graph weights] [graph (->> graph
                                       (mapcat (fn [[k vs]]
                                                 (->> vs
                                                      (map (fn [v] #{k v})))))
                                       set
                                       (map (fn [e] [e 1]))
                                       (into {}))]
           [best-w best-part] [Long/MAX_VALUE #{}]
           part #{}]
      (if (= 1 (count graph))
        best-part
        (let [[s t w] (step graph weights)
              new-part (conj part t)]
          (prn [(lib/now) (lib/duration-since start-time) (count graph) best-w (->> weights vals (reduce + 0)) (get weights #{s t})])
          (recur (merge-vertices graph weights s t)
                 (if (< w best-w)
                   [w new-part]
                   [best-w best-part])
                 new-part))))))

(defn part1
  [input]
  (let [part (stoer-wagner input)
        c (count part)]
    (prn [part (count part) (set/difference (->> input keys set) part) (count (set/difference (->> input keys set) part)) (- (count input) c)
          (->> input
               (mapcat (fn [[k vs]] (->> vs (map (fn [v] #{k v})))))
               set
               (filter (fn [s] (->> s (filter part) count (= 1)))))
          ])
    (* c (- (count input) c))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 54
  [part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
