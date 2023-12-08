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
                      (let [[_ start left right] (re-find #"([A-Z0-9]+) = \(([A-Z0-9]+), ([A-Z0-9]+)\)" line)]
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
  [{:keys [directions nodes]}]
  (loop [s 0
         ds (cycle directions)
         pos (->> nodes keys (filter (fn [s] (= \A (get s 2)))))]
    (if (every? (fn [s] (= \Z (get s 2))) pos)
      s
      (recur (inc s)
             (rest ds)
             (->> pos
                  (mapv (fn [p] (get-in nodes [p (first ds)]))))))))

(lib/check
  [part1 sample1] 2
  [part1 puzzle] 15517
  [part2 sample2] 6
  [part2 puzzle] 0)
