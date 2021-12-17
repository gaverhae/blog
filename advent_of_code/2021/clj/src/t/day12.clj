(ns t.day12
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn parse
  [lines]
  (->> lines
       (map #(string/split % #"-"))
       (mapcat (fn [[a b]] [{a #{b}} {b #{a}}]))
       (apply merge-with set/union {})))

(defn small?
  [^String s]
  (= s (.toLowerCase s)))

(defn ends?
  [[pos]]
  (= pos "end"))

(defn part1
  [input]
  (loop [num-paths 0
         paths [["start" #{"start"}]]]
    (if (empty? paths)
      num-paths
      (let [p (for [[pos visited] paths
                    next-step (get input pos)
                    :when (not (visited next-step))]
                [next-step
                 (if (small? next-step)
                   (conj visited next-step)
                   visited)])]
        (recur (->> p (filter ends?) count (+ num-paths))
               (->> p (remove ends?)))))))

(defn part2
  [input]
  (loop [num-paths 0
         paths [["start" #{"start"} false]]]
    (if (empty? paths)
      num-paths
      (let [p (for [[pos visited twice?] paths
                    next-step (get input pos)
                    :when (or (not (visited next-step))
                              (and (not= "start" next-step)
                                   (not twice?)))]
                [next-step
                 (if (small? next-step)
                   (conj visited next-step)
                   visited)
                 (or twice?
                     (boolean (visited next-step)))])]
        (recur (->> p (filter ends?) count (+ num-paths))
               (->> p (remove ends?)))))))
