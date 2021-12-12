(ns t.day12
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [t.util :refer [parse-integers transpose]]))

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
  [[path _]]
  (= (last path) "end"))

(defn part1
  [input]
  (loop [num-paths 0
         paths [[["start"] #{"start"}]]]
    (if (empty? paths)
      num-paths
      (let [p (for [[path visited] paths
                    next-step (get input (last path))
                    :when (not (visited next-step))]
                [(conj path next-step)
                 (if (small? next-step)
                   (conj visited next-step)
                   visited)])]
        (recur (->> p (filter ends?) count (+ num-paths))
               (->> p (remove ends?)))))))

(defn part2
  [input]
  (loop [num-paths 0
         paths [[["start"] #{"start"} false]]]
    (if (empty? paths)
      num-paths
      (let [p (for [[path visited twice?] paths
                    next-step (get input (last path))
                    :when (or (not (visited next-step))
                              (and (not= "start" next-step)
                                   (not twice?)))]
                [(conj path next-step)
                 (if (small? next-step)
                   (conj visited next-step)
                   visited)
                 (or twice?
                     (boolean (visited next-step)))])]
        (recur (->> p (filter ends?) count (+ num-paths))
               (->> p (remove ends?)))))))
