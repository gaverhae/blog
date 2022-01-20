(ns t.core
  (:require [taoensso.tufte :as tufte :refer (p profiled)]
            [clojure.string :as string]
            [clojure.set :as set])
  (:gen-class))

(defn parse
  [lines]
  (->> lines
       (map #(string/split % #"-"))
       (mapcat (fn [[a b]] [{a #{b}} {b #{a}}]))
       (apply merge-with set/union {})))

(defn small?
  [^String s]
  (p :small? (= s (.toLowerCase s))))

(defn ends?
  [[path _]]
  (p :ends? (= (last path) "end")))

(defn part2
  [input]
  (loop [num-paths 0
         paths [[["start"] #{"start"} false]]]
    (if (empty? paths)
      num-paths
      (let [path (for [[path visited twice?] paths
                       next-step (p :next-step
                                    (get input (last path)))
                       :when (p :when
                                (or (not (visited next-step))
                                    (and (not= "start" next-step)
                                         (not twice?))))]
                   [(p :conj-path (conj path next-step))
                    (p :visited
                       (if (small? next-step)
                         (conj visited next-step)
                         visited))
                    (p :twice-check
                       (or twice?
                           (boolean (visited next-step))))])]
        (recur (p :filter-path (->> path (filter ends?) count (+ num-paths)))
               (p :remove-ends (->> path (remove ends?))))))))

(defn -main
  [& args]
  (let [input (-> (slurp "day12")
                  (string/split-lines)
                  parse)]
    (->> (profiled {} (part2 input))
         second
         deref
         pprint)))
