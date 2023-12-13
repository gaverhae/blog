(ns t.day13
  (:require [clojure.core.async :as async]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  (->> lines
       (partition-by #{""})
       (remove #{[""]})))

(defn find-pal
  [pat]
  (first (for [pos (range 1 (count pat))
               :let [[up down] [(take pos pat) (drop pos pat)]
                     size (min (count up) (count down))
                     [up down] (if (> (count up) (count down))
                                 [(drop (- pos size) up) down]
                                 [up (take size down)])]
               :when (= up (reverse down))]
           pos)))

(defn part1
  [input]
  (->> input
       (map (fn [pat]
              (if-let [p (find-pal pat)]
                (* 100 p)
                (find-pal (lib/transpose pat)))))
       (reduce + 0)))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 405
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
