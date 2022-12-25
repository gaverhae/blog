(ns t.day25
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn snafu->dec
  [s]
  (->> s
       reverse
       (map-indexed vector)
       (reduce (fn [acc [idx c]]
                 (+ acc (* (long (Math/pow 5 idx))
                           ({\1 1, \2 2, \0 0, \- -1, \= -2} c))))
               0)))

(defn parse
  [lines]
  (->> lines (map snafu->dec)))

(comment

(/ (Math/log 35798042807410) (Math/log 5))
19.39118874766443

(* 2 (long (Math/pow 5 19)))
(- 38146972656250
   35798042807410)

(long (/ (Math/log 2348929848840) (Math/log 5)))
17

(- 95367431640625 (* 2 (long (Math/pow 5 18))))

(- 35798042807410
   (snafu->dec
     "2-20=01--0=0=0=2-120"))


(defn dec->snafu
  [n]
  (let [max-idx (long (Math/floor (/ (Math/log n) (Math/log 5))))]
    (if (>= n
  (->> (str n)
       reverse
       (map (fn [acc [idx c]])))))))

)

(defn part1
  [input]
  (prn (- 35798042807410 (snafu->dec "2-000000000000000000")))
  (reduce + 0 input))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 4890
  [part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
