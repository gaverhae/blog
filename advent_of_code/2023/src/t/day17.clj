(ns t.day17
  (:require [clojure.core.async :as async]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  (->> lines
       (mapv (fn [line]
               (->> line
                    (mapv (comp parse-long str)))))))

(defn part1
  [input]
  (let [end [(dec (count input)) (dec (count (first input)))]
        left [0 -1] right [0 1] up [-1 0] down [1 0]
        h (fn [p] (lib/manhattan end p))]
    (loop [to-visit [[(h [0 0]) 0 [0 0] 0]
                     [(h [0 0]) 0 [0 0] 1]]
           visited? #{}]
      (if (empty? to-visit)
        :error
        (let [[g c p dir] (first to-visit)]
          (if (= end p)
            c
            (recur (->> (for [dist (range 1 4)
                              sign [+ -]
                              :let [new-p (update p dir sign dist)]
                              :when (get-in input new-p)]
                          [new-p
                           (reduce (fn [acc el]
                                     (+ acc (get-in input (update p dir sign (inc el)))))
                                   c
                                   (range dist))])
                        (map (fn [[p c]] [(+ c (h p)) c p (- 1 dir)]))
                        (concat to-visit)
                        (remove (fn [[g c p dir]] (visited? [p dir])))
                        (sort-by first))
                   (conj visited? [p dir]))))))))

(defn part2
  [input]
  (let [end [(dec (count input)) (dec (count (first input)))]
        left [0 -1] right [0 1] up [-1 0] down [1 0]
        h (fn [p] (lib/manhattan end p))]
    (loop [to-visit [[(h [0 0]) 0 [0 0] 0]
                     [(h [0 0]) 0 [0 0] 1]]
           visited? #{}]
      (if (empty? to-visit)
        :error
        (let [[g c p dir] (first to-visit)]
          (if (= end p)
            c
            (recur (->> (for [dist (range 4 11)
                              sign [+ -]
                              :let [new-p (update p dir sign dist)]
                              :when (get-in input new-p)]
                          [new-p
                           (reduce (fn [acc el]
                                     (+ acc (get-in input (update p dir sign (inc el)))))
                                   c
                                   (range dist))])
                        (map (fn [[p c]] [(+ c (h p)) c p (- 1 dir)]))
                        (concat to-visit)
                        (remove (fn [[g c p dir]] (visited? [p dir])))
                        (sort-by first))
                   (conj visited? [p dir]))))))))

(lib/check
  #_#_[part1 sample] 102
  #_#_[part1 puzzle] 758
  [part2 sample] 94
  [part2 sample1] 71
  [part2 puzzle] 0)
