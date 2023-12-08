(ns t.day08
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(defn parse
  [lines]
  {:directions (->> (first lines)
                    (map {\L 0, \R 1}))
   :nodes (->> (drop 2 lines)
               (map (fn [line]
                      (let [[_ start left right] (re-find #"([A-Z]+) = \(([A-Z]+), ([A-Z]+)\)" line)]
                        [start [left right]])))
               (into {}))})

(defn part1
  [{:keys [directions nodes]}]
  (loop [step 0
         directions (cycle directions)
         pos "AAA"]
    (if (= pos "ZZZ")
      step
      (recur (inc step)
             (rest directions)
             (get-in nodes [pos (first directions)])))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 2
  [part1 puzzle] 15517
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
