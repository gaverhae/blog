(ns t.day01
  (:require [t.lib :as lib]
            [clojure.string :as string]
            [clojure.test :refer [deftest are]]))

(def parse lib/parse-integers)

(defn part1
  [input]
  0)

(defn part2
  [input]
  0)

(deftest tests
  (are [expected input f]
       (= expected (-> (slurp (str "data/day01-" input))
                       string/split-lines
                       parse
                       f))
       0 "sample" part1
       ;0 "full" part1
       ;0 "sample" part2
       ;0 "full" part2
       ))
