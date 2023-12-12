(ns t.day12
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [[symbols bounds] (s/split line #" ")]
                [symbols
                 (->> (re-seq #"\d+" bounds)
                      (map parse-long))])))))

(defn part1
  [input]
  (let [matches? (fn [pattern]
                   (fn [line]
                     (->> line
                          (re-seq #"#+")
                          (map count)
                          (= pattern))))]
    (->> input
         (map (fn [[symbols pattern]]
                (->> (loop [to-process symbols
                            processed []]
                       (if (empty? to-process)
                         processed
                         (let [s (first to-process)
                               to-process (rest to-process)]
                           (recur to-process
                                  (->> (if (= \? s) [\. \#] [s])
                                       (mapcat (fn [new-s]
                                                 (if (empty? processed)
                                                   [new-s]
                                                   (->> processed
                                                        (map (fn [prev] (str prev new-s))))))))))))
                     (filter (matches? pattern))
                     count)))
         (reduce + 0))))

(defn part2
  [input]
  (->> input
       (map (fn [[symbols pattern]]
              [(apply str (interpose \? (repeat 5 symbols)))
               (apply concat (repeat 5 pattern))]))
       ))

(lib/check
  [part1 sample] 21
  [part1 puzzle] 7090
  #_#_[part2 sample] 525152
  #_#_[part2 puzzle] 0)
