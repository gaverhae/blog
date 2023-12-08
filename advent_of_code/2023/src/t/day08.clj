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

(defn solve
  [start dirs nodes]
  (loop [step 0
         dirs (cycle dirs)
         pos start]
    (if (= \Z (get pos 2))
      step
      (recur (inc step)
             (rest dirs)
             (get-in nodes [pos (first dirs)])))))

(defn part1
  [{:keys [directions nodes]}]
  (solve "AAA" directions nodes))

(defn part2
  [{:keys [directions nodes]}]
  (->> nodes
       keys
       (filter (fn [s] (= \A (get s 2))))
       (map (fn [s] (solve s directions nodes)))
       (reduce lib/lcm)))

(lib/check
  [part1 sample1] 2
  [part1 puzzle] 15517
  [part2 sample2] 6
  [part2 puzzle] 14935034899483)
