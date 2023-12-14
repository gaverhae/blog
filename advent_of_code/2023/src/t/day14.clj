(ns t.day14
  (:require [clojure.core.async :as async]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  lines)

(defn part1
  [input]
  (->> (cons (repeat (count (first input)) \#)
             input)
       lib/transpose
       (map (fn [col]
              (->> (apply str col)
                   (re-seq #"#+[.O]*")
                   (mapcat (fn [s]
                             (let [f (frequencies s)]
                               (concat (repeat (f \#) \#)
                                       (repeat (f \O 0) \O)
                                       (repeat (f \. 0) \,)))))
                   reverse
                   (apply str))))
       (map (fn [line]
              (loop [idx 0
                     ld 0]
                (if (== idx (count line))
                  ld
                  (recur (inc idx)
                         (+ (if (= \O (get line idx)) (inc idx) 0)
                            ld))))))
       (reduce + 0)))


(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 136
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
