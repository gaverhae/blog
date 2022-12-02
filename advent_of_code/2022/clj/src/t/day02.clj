(ns t.day02
  (:require [t.lib :as lib]
            [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [clojure.test :refer [deftest are]]))

(defn parse
  [input]
  input)

(defn part1
  [input]
  (->> input
       (map (fn [s] [({\A :rock, \B :paper, \C :scissors} (get s 0))
                     ({\X :rock, \Y :paper, \Z :scissors} (get s 2))]))
       (map (fn [[a b]]
              (match [a b]
                [:rock :rock] (+ 1 3)
                [:rock :paper] (+ 2 6)
                [:rock :scissors] (+ 3 0)
                [:paper :rock] (+ 1 0)
                [:paper :paper] (+ 2 3)
                [:paper :scissors] (+ 3 6)
                [:scissors :rock] (+ 1 6)
                [:scissors :paper] (+ 2 0)
                [:scissors :scissors] (+ 3 3))))
       (reduce + 0)))

(defn part2
  [input]
  (->> input
       (map (fn [s] [({\A :rock, \B :paper, \C :scissors} (get s 0))
                     ({\X 0, \Y 3, \Z 6} (get s 2))]))
       (map (fn [[a b]]
              (match [a b]
                [:rock 0] (+ 0 3)
                [:rock 3] (+ 3 1)
                [:rock 6] (+ 6 2)
                [:paper 0] (+ 0 1)
                [:paper 3] (+ 3 2)
                [:paper 6] (+ 6 3)
                [:scissors 0] (+ 0 2)
                [:scissors 3] (+ 3 3)
                [:scissors 6] (+ 6 1))))
       (reduce + 0)))

(deftest tests
  (are [expected input f]
       (= expected (-> (slurp (str "data/day02-" input))
                       string/split-lines
                       parse
                       f))
       15 "sample" part1
       13268 "full" part1
       12 "sample" part2
       15508 "full" part2
       ))
