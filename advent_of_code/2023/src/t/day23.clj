(ns t.day23
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
  {:grid lines
   :start [0 (s/index-of (first lines) \.)]
   :end [(dec (count lines)) (s/index-of (last lines) \.)]})

(defn part1
  [input]
  (let [{:keys [start end grid]} input]
    (loop [todo [[0 start #{start}]]
           best-cost-so-far 0]
      (if (empty? todo)
        best-cost-so-far
        (let [[[cost [y x :as pos] seen?] & todo] todo]
          (recur (->> (case (get-in grid pos)
                        \> [[0 1]]
                        \^ [[-1 0]]
                        \v [[1 0]]
                        \< [[0 -1]]
                        \. [[0 1] [0 -1] [1 0] [-1 0]])
                      (map (fn [[dy dx]] [(+ y dy) (+ x dx)]))
                      (remove seen?)
                      (remove (fn [p] (= \# (get-in grid p \#))))
                      (map (fn [p] [(inc cost) p (conj seen? p)]))
                      (concat todo)
                      (sort-by first)
                      reverse)
                 (cond-> best-cost-so-far
                   (= end pos)
                   (max cost))))))))

(defn part2
  [input]
  (let [{:keys [start end grid]} input
        start-time (lib/now-millis)]
    (loop [todo [[0 start #{start}]]
           best-cost-so-far 0
           prev-cost 0
           step 0]
      (when (not= best-cost-so-far prev-cost)
        (println (format "%s[%12d]: %8d (%8.2f steps/ms)"
                         (lib/duration-since start-time)
                         step
                         best-cost-so-far
                         (/ step (- (lib/now-millis) start-time) 1.0))))
      (if (empty? todo)
        best-cost-so-far
        (let [[[cost [y x :as pos] seen?] & todo] todo]
          (recur (->> [[0 1] [0 -1] [1 0] [-1 0]]
                      (map (fn [[dy dx]] [(+ y dy) (+ x dx)]))
                      (remove seen?)
                      (remove (fn [p] (= \# (get-in grid p \#))))
                      (map (fn [p] [(inc cost) p (conj seen? p)]))
                      (reduce conj todo))
                 (cond-> best-cost-so-far
                   (= end pos)
                   (max cost))
                 best-cost-so-far
                 (inc step)))))))

(lib/check
  [part1 sample] 94
  [part1 puzzle] 2202
  [part2 sample] 154
  [part2 puzzle] 0)
