(ns t.day15
  (:require [clojure.core.async :as async]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  (-> lines
      first
      (s/split #",")))

(defn h
  [word]
  (->> word
       (map int)
       (reduce (fn [acc el]
                 (-> acc
                     (+ el)
                     (* 17)
                     (rem 256)))
               0)))

(defn part1
  [input]
  (->> input
       (map h)
       (reduce + 0)))

(defn part2
  [input]
  (->> input
       (map (fn [word]
              (let [label (take 2 word)
                    box (h label)
                    op (get word 2)
                    arg (drop 3 word)]
                (if (= op \=)
                  [:= box (apply str label) (parse-long (apply str arg))]
                  [:- box (apply str label)]))))
       (reduce (fn [acc op]
                 (case (first op)
                   := (let [[_ box label n] op]
                        (update acc box (fn [lenses]
                                          (if (some #(= label (first %)) lenses)
                                            (mapv (fn [lens]
                                                    (if (= label (first lens))
                                                      [label n]
                                                      lens))
                                                  lenses)
                                            (conj (or lenses []) [label n])))))
                   :- (let [[_ box label] op]
                        (update acc box (fn [lenses]
                                          (->> lenses
                                               (remove #(= label (first %)))
                                               vec))))))
               {})
       (mapcat (fn [[box lenses]]
                 (->> lenses
                      (map-indexed (fn [slot [label focus]]
                                     (* (inc box)
                                        (inc slot)
                                        focus))))))
       (reduce + 0)))

(lib/check
  [part1 sample1] 52
  [part1 sample] 1320
  [part1 puzzle] 501680
  [part2 sample] 145
  [part2 puzzle] 0)
