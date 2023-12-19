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
  [[workflows _]]
  (loop [states [{:parts {:x [1 4000], :m [1 4000], :a [1 4000], :s [1 4000]}
                  :workflow :in}]
         counted 0]
    (if (empty? states)
      counted
      (let [[{:keys [parts workflow]} & states] states]
        (case workflow
          :A (recur states (+ counted
                              (->> parts
                                   vals
                                   (map (fn [[from to]] (inc (- to from))))
                                   (reduce * 1))))
          :R (recur states counted)
          (recur (reduce conj
                         states
                         (loop [rules (get workflows workflow)
                                parts parts
                                new-states []]
                           (if (nil? parts)
                             new-states
                             (match (first rules)
                               [:> a v r] (let [[from-a to-a] (get parts a)]
                                            (cond (< v from-a)(recur () nil (conj new-states {:parts parts, :workflow r}))
                                                  (< to-a v) (recur (rest rules) parts new-states)
                                                  (<= from-a v to-a)
                                                  (recur (rest rules)
                                                         (assoc parts a [from-a v])
                                                         (conj new-states {:parts (assoc parts a [(inc v) to-a])
                                                                           :workflow r}))))
                               [:< a v r] (let [[from-a to-a] (get parts a)]
                                            (cond (< v from-a) (recur (rest rules) parts new-states)
                                                  (< to-a v) (recur () nil (conj new-states {:parts parts, :workflow r}))
                                                  (<= from-a v to-a)
                                                  (recur (rest rules)
                                                         (assoc parts a [v to-a])
                                                         (conj new-states {:parts (assoc parts a [from-a (dec v)])
                                                                           :workflow r}))))
                               [:ref r] (recur nil nil (conj new-states {:parts parts, :workflow r}))))))
                 counted))))))

(lib/check
  [part1 sample] 19114
  [part1 puzzle] 402185
  [part2 sample] 167409079868000
  [part2 puzzle] 130291480568730)
