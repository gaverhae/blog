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
  (let [cycle-lengths (->> nodes keys (filter (fn [s] (= \A (get s 2))))
                           (map (fn [start]
                                  (loop [step 0
                                         directions (cycle directions)
                                         pos start]
                                    (if (= \Z (get pos 2))
                                      step
                                      (recur (inc step)
                                             (rest directions)
                                             (get-in nodes [pos (first directions)]))))))
                           vec)]
    cycle-lengths
    #_(loop [c cycle-lengths]
      (prn [:c c])
      (if (apply = c)
        (first c)
        (let [min-idx (->> c
                           (map-indexed vector)
                           (some (fn [[idx v]] (when (= v (first (sort c))) idx))))]
          (recur (update (vec c) min-idx + (get cycle-lengths min-idx)))))))

  #_(loop [s 0
         ds (cycle directions)
         pos (->> nodes keys (filter (fn [s] (= \A (get s 2)))))]
    (if (every? (fn [s] (= \Z (get s 2))) pos)
      s
      (recur (inc s)
             (rest ds)
             (->> pos
                  (mapv (fn [p] (get-in nodes [p (first ds)]))))))))

(lib/check
  #_#_[part1 sample1] 2
  #_#_[part1 puzzle] 15517
  [part2 sample2] 6
  [part2 puzzle] 14935034899483)
