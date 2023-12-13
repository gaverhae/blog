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

(defn find-pal-smudge
  [pat]
  (first (for [pos (range 1 (count pat))
               :let [[up down] [(take pos pat) (drop pos pat)]
                     size (min (count up) (count down))
                     [up down] (if (> (count up) (count down))
                                 [(drop (- pos size) up) down]
                                 [up (take size down)])]
               :when (->> (map vector up (reverse down))
                          (map (fn [[line-up line-bottom]]
                                 (->> (map vector line-up line-bottom)
                                      (keep (fn [[a b]] (when (not= a b) 1)))
                                      (reduce + 0))))
                          (reduce + 0)
                          (= 1))]
           pos)))

(defn part2
  [input]
  (->> input
       (map (fn [pat]
              (if-let [p (find-pal-smudge pat)]
                (* 100 p)
                (find-pal-smudge (lib/transpose pat)))))
       (reduce + 0)))

(lib/check
  [part1 sample] 405
  [part1 puzzle] 31956
  [part2 sample] 400
  [part2 puzzle] 37617)
