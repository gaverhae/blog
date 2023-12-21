(ns t.day21
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  {:grid lines
   :start (->> lines
               (map-indexed (fn [y line]
                              (->> line
                                   (keep-indexed (fn [x c]
                                                   (when (= c \S)
                                                     [y x]))))))
               (apply concat)
               first)})

(defn part1
  [input max-steps]
  (loop [step 0
         ps [(:start input)]]
    (println ps)
    (if (= max-steps step)
      (count ps)
      (recur (inc step)
             (->> ps
                  (mapcat (fn [[y x]]
                            (for [[dy dx] [[-1 0] [1 0] [0 1] [0 -1]]
                                  :let [y (+ y dy)
                                        x (+ x dx)]
                                  :when (#{\S \.} (get-in input [:grid y x]))]
                              [y x])))
                  set)))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample 6] 16
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
