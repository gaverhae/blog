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

(defn expand
  [input]
  (let [input (set input)
        [minx maxx miny maxy minz maxz]
        (reduce (fn [[minx maxx miny maxy minz maxz] [x y z]]
                  [(min x minx) (max x maxx)
                   (min y miny) (max y maxy)
                   (min z minz) (max z maxz)])
                [0 0 0 0 0 0]
                input)]
    (loop [steam #{[0 0 0]}]
      (let [after (set/difference
                    (->> steam
                         (mapcat (fn [[x y z]]
                                   [[(inc x) y z]
                                    [(dec x) y z]
                                    [x (inc y) z]
                                    [x (dec y) z]
                                    [x y (inc z)]
                                    [x y (dec z)]]))
                         (filter (fn [[x y z]]
                                   (and (<= (dec minx) x (inc maxx))
                                        (<= (dec miny) y (inc maxy))
                                        (<= (dec minz) z (inc maxz)))))
                         (concat steam)
                         set)
                    input)]
        (if (= steam after)
          steam
          (recur after))))))

(defn part2
  [input]
  (count (set/intersection
           (->> input (mapcat cube->faces) set)
           (->> (expand input) (mapcat cube->faces) set))))

(lib/check
  #_#_[part1 sample] 64
  #_#_[part1 puzzle] 4302
  #_#_[part2 sample] 58
  #_#_[part2 puzzle] 2492)

