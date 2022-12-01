(ns t.day02
  (:require [t.lib :as lib]
            [clojure.string :as string]
            [clojure.test :refer [deftest are]]))

(defn parse
  [input]
  )

(defn part1
  [input]
  )

(defn part2
  [input]
  )

(deftest tests
  (are [expected input f]
       (= expected (-> (slurp (str "data/day02-" input))
                       string/split-lines
                       parse
                       f))
       0 "sample" part1
       ;0 "full" part1
       ;0 "sample" part2
       ;0 "full" part2
       ))
