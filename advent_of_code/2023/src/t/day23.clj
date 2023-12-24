(ns t.day23
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
                 (long (cond-> best-cost-so-far
                         (= end pos)
                         (max cost)))))))))

(defn part2
  [input]
  (let [{:keys [start end grid]} input
        h (count grid)
        w (count (first grid))
        to-int (fn [[y x]] (+ (* w y) x))
        end (to-int end)
        start (to-int start)
        neighbours (->> grid
                        (map-indexed
                          (fn [y line]
                            (->> line
                                 (map-indexed (fn [x c]
                                                [(to-int [y x])
                                                 (->> [[0 1] [0 -1] [1 0] [-1 0]]
                                                      (map (fn [[dy dx]] [(+ y dy) (+ x dx)]))
                                                      (remove (fn [p] (= \# (get-in grid p \#))))
                                                      (map to-int)
                                                      vec)])))))
                        (apply concat)
                        (into {}))
        ;; TODO: merge contiguous segments
        neighs ^"[[J" (make-array Long/TYPE (* h w) 0)
        _ (doseq [[k vs] neighbours]
            (aset neighs k ^"[J" (into-array Long/TYPE vs)))
        start-time (lib/now-millis)]
    (loop [todo [[0 start (conj (i/int-set) start)]]
           best-cost-so-far 0
           prev-cost 0
           step 0]
      (if #_(or (empty? todo) (= (* 20 1000 1000) step))
        (empty? todo)
        (do
          (println (format "%s[%12d]: %8d (%8.2f steps/ms)"
                           (lib/duration-since start-time)
                           step
                           best-cost-so-far
                           (/ step
                              (let [t (- (lib/now-millis) start-time)]
                                (or (and (pos? t) t)
                                    1))
                              1.0)))
          best-cost-so-far)
        (let [[[cost pos seen?] & todo] todo]
          (recur (->> (aget neighs pos)
                      (remove seen?)
                      (map (fn [p] [(inc cost) p (conj seen? p)]))
                      (reduce conj todo))
                 (long (cond-> best-cost-so-far
                         (= end pos) (max cost)))
                 best-cost-so-far
                 (inc step)))))))

(lib/check
  #_#_[part1 sample] 94
  #_#_[part1 puzzle] 2202
  #_#_[part2 sample] 154
  #_#_[part2 puzzle] 6226)
