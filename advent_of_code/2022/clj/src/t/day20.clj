(ns t.day20
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (mapv ->long)))

(defn rotate
  [acc el]
  (let [len (count acc)
        current-index (.indexOf ^java.util.List acc el)
        target-index (let [t (+ el current-index)]
                       (cond (neg? t) (dec (mod t len))
                             (>= t len) (inc (mod t len))
                             :else t))
        steps (Math/abs (long (- current-index target-index)))]
    (if (> current-index target-index)
      (vec (concat (subvec acc 0 target-index)
                   [el]
                   (subvec acc target-index current-index)
                   (subvec acc (inc current-index))))
      (vec (concat (subvec acc 0 current-index)
                   (subvec acc (inc current-index) (inc target-index))
                   [el]
                   (subvec acc (inc target-index)))))))

(defn part1
  [input]
  (let [v (reduce rotate input input)
        zero-idx (.indexOf ^java.util.List v 0)]
    (+ (v (mod (+ zero-idx 1000) (count v)))
       (v (mod (+ zero-idx 2000) (count v)))
       (v (mod (+ zero-idx 3000) (count v))))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 3
  [part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
