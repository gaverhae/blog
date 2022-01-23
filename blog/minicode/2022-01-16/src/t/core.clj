(ns t.core
  (:require [taoensso.tufte :as tufte :refer [p]]
            [criterium.core :as crit]
            [clojure.string :as string]
            [clojure.set :as set])
  (:gen-class))

(defn parse
  [lines]
  (->> lines
       (map #(string/split % #"-"))
       (mapcat (fn [[a b]] [{a #{b}} {b #{a}}]))
       (apply merge-with set/union {})))

(defn p-small?
  [^String s]
  (p :small? (= s (.toLowerCase s))))

(defn small?
  [^String s]
  (= s (.toLowerCase s)))

(defn p-ends?
  [[path _]]
  (p :ends? (= (last path) "end")))

(defn ends?
  [[path _]]
  (= (last path) "end"))

(defn p-part2
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
                       (if (p-small? next-step)
                         (conj visited next-step)
                         visited))
                    (p :twice-check
                       (or twice?
                           (boolean (visited next-step))))])]
        (recur (p :filter-path
                  (->> path (filter p-ends?) count (+ num-paths)))
               (p :remove-ends
                  (->> path (remove p-ends?))))))))
(defn part2
  [input]
  (loop [num-paths 0
         paths [[["start"] #{"start"} false]]]
    (if (empty? paths)
      num-paths
      (let [path (for [[path visited twice?] paths
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
        (recur (->> path (filter ends?) count (+ num-paths))
               (->> path (remove ends?)))))))

(defn bench
  [f]
  (->> (crit/benchmark (f) {}) :mean first))

(defn -main
  [& args]
  (tufte/add-handler! :my-custom-dev-null (fn [m]))
  (let [input (-> (slurp "day12")
                  (string/split-lines)
                  parse)]
    (doseq [[tag f] [["no perf log" #(part2 input)]
                     ["disabled" #(p-part2 input)]
                     ["enabled" #(tufte/profile {} (p-part2 input))]]]
      (println (format "%15s: %.3e" tag (bench f))))))
