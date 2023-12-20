(ns t.day20
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(def parser
  (insta/parser
    "
    <S> = type name <' -> '> output
    type = '&' | '%' | ''
    <name> = #'[a-z]+'
    <output> = name (<', '> name)*
    "))

(defn parse
  [lines]
  (->> lines
       (map parser)
       (map (fn [[[_ t] n & outs]]
              {:type ({"&" :conj, "%" :flip, nil :b} t)
               :name n
               :outputs outs}))))

(defn part1
  [input]
  input)

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 32000000
  [part1 sample1] 11687500
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
