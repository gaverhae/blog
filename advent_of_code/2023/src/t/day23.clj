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
  [input stop-m]
  input)

(lib/check
  [part1 sample] 94
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
