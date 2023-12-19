(ns t.day19
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(def rules-parser
  (insta/parser
    "<S> = name <'{'> rule (<','> rule)* <'}'>
    <name> = #'[a-z]+' | 'A' | 'R'
    <rule> = compare | ref
    compare = (attr comp int <':'> name)
    ref = name
    <attr> = 'a' | 'x' | 'm' | 's'
    <comp> = '<' | '>'
    <int> = #'[0-9]+'"))

(def part-parser
  (insta/parser
    "<S> = <'{'> attr (<','> attr)+ <'}'>
    attr = #'[a-z]' <'='> #'[0-9]+'"))

(defn parse
  [lines]
  (let [[rules parts] (->> lines
                           (partition-by #{""})
                           (remove #{[""]}))]
    [(->> rules
          (map rules-parser)
          (map (fn [[rn & rules]]
                 [(keyword rn)
                  (->> rules
                       (map (fn [rule]
                              (case (first rule)
                                :compare (let [[_ a c v r] rule]
                                           [(keyword c)
                                            (keyword a)
                                            (parse-long v)
                                            (keyword r)])
                                :ref [:ref (keyword (second rule))]))))]))
          (into {}))
     (->> parts
          (map part-parser)
          (map (fn [part]
                 (->> part
                      (map (fn [[_ k v]]
                             [(keyword k)
                              (parse-long v)]))
                      (into {})))))]))

(defn part1
  [[rules parts]]
  (->> parts
       (map (fn [part]
              (loop [workflow :in]
                (let [r (loop [rules (get rules workflow)]
                          (let [rule (first rules)
                                res (case (first rule)
                                      :> (let [[_ a v r] rule] (when (> (get part a) v) r))
                                      :< (let [[_ a v r] rule] (when (< (get part a) v) r))
                                      :ref (let [[_ r] rule] r))]
                            (case res
                              :A (->> part vals (reduce + 0))
                              :R 0
                              nil (recur (rest rules))
                              res)))]
                  (if (integer? r)
                    r
                    (recur r))))))
       (reduce + 0)))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 19114
  [part1 puzzle] 402185
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
