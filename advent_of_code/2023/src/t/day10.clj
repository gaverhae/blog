(ns t.day10
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(defn parse
  [lines]
  (vec lines))

(defn first-connected-neighbour
  [[y0 x0] grid seen?]
  (first (for [[y x c1 c2] [
                            [(inc y0) x0 #{\| \7 \F \S} #{\| \L \J \S}]
                            [(dec y0) x0 #{\| \L \J \S} #{\| \7 \F \S}]
                            [y0  (dec x0) #{\- \J \7 \S} #{\- \L \F \S}]
                            [y0  (inc x0) #{\- \F \L \S} #{\- \J \7 \S}]
                            ]
               :let [pipe (get-in grid [y x])]
               :when (and pipe
                          (c1 (get-in grid [y0 x0]))
                          (c2 pipe)
                          (not (seen? [y x])))]
           [y x])))

(defn part1
  [input]
  (let [start (->> input
                   (keep-indexed (fn [y line]
                                   (->> line (keep-indexed (fn [x c] (when (= \S c) [y x]))))))
                   (apply concat)
                   first)]
    (loop [step 0
           so-far #{}
           pos start]
      (if (and (pos? step) (= start pos))
        (quot step 2)
        (let [pos (first-connected-neighbour pos input so-far)]
          (recur (inc step)
                 (conj so-far pos)
                 pos))))))

(defn part2
  [input])

(lib/check
  #_#_[part1 sample] 8
  [part1 puzzle] 6968
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
