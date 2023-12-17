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
    (loop [to-visit [[(h [0 0]) 0 [0 0] ()]]
           visited? #{}]
      (if (empty? to-visit)
        :error
        (let [[g c p hist] (first to-visit)]
          (if (= end p)
            c
            (recur (->> (rest to-visit)
                        (concat (->> [left right up down]
                                     (remove (fn [[dy dx]]
                                               (when-let [[last-dy last-dx] (first hist)]
                                                 (= [dy dx] [(- last-dy) (- last-dx)]))))
                                     (remove (fn [[dy dx]]
                                               (= (repeat 3 [dy dx])
                                                  (take 3 hist))))
                                     (keep (fn [[dy dx]]
                                             (let [[y x] p
                                                   y (+ y dy)
                                                   x (+ x dx)]
                                               (when-let [loss (get-in input [y x])]
                                                 [(+ loss c (h [y x]))
                                                  (+ loss c)
                                                  [y x]
                                                  (cons [dy dx] hist)]))))))
                        (remove (fn [[g c p hist]]
                                  (visited? [p (take 3 hist)])))
                        (sort-by first))
                   (conj visited? [p (take 3 hist)]))))))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 102
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
