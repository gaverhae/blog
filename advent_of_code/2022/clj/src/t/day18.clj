(ns t.day18
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line] (mapv ->long (string/split line #","))))))

(defn cube->faces
  [cube]
  (let [inc (fn [d] (+ d (/ 1 2)))
        dec (fn [d] (- d (/ 1 2)))]
    (for [p [0 1 2]
          d [inc dec]]
      (let [[a-idx b-idx] (get [[1 2] [0 2] [0 1]] p)
            base (update cube p d)]
        (->> (for [a [inc dec] b [inc dec]] [a b])
             (mapv (fn [[a b]]
                     (-> base
                         (update a-idx a)
                         (update b-idx b)))))))))

(defn part1
  [input]
  (->> input
       (mapcat cube->faces)
       (frequencies)
       (filter (fn [[v n]] (== 1 n)))
       count))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 64
  [part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0
  )

