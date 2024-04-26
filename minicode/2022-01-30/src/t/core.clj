(ns t.core
  (:require [criterium.core :as crit]
            [clojure.string :as string]
            [clojure.set :as set])
  (:gen-class))

(defn parse
  [lines]
  (->> lines
       (map #(string/split % #"-"))
       (mapcat (fn [[a b]] [{a #{b}} {b #{a}}]))
       (apply merge-with set/union {})
       (reduce (fn [m [k v]]
                 (assoc m k (into () v)))
               {})))

(defn small?
  [^String s]
  (= s (.toLowerCase s)))

(defn part2
  [input]
  (loop [num-paths 0
         paths [["start" #{"start"} false]]
         next-steps ()
         visited #{}
         twice? false]
    (cond (and (empty? next-steps) (empty? paths))
          num-paths
          (empty? next-steps)
          (let [[pos visited twice?] (first paths)]
            (recur num-paths (rest paths) (get input pos) visited twice?))
          (= (first next-steps) "end")
          (recur (inc num-paths) paths (rest next-steps) visited twice?)
          :else
          (let [next-step (first next-steps)]
            (if (or (not (visited next-step))
                    (and (not= "start" next-step)
                         (not twice?)))
              (recur num-paths
                     (conj paths [next-step
                                  (if (small? next-step)
                                    (conj visited next-step)
                                    visited)
                                  (or twice?
                                      (boolean (visited next-step)))])
                     (rest next-steps)
                     visited
                     twice?)
              (recur num-paths paths (rest next-steps) visited twice?))))))

(defn bench
  [f]
  (->> (crit/benchmark (f) {}) :mean first))

(defn -main
  [& args]
  (let [input (-> (slurp "day12")
                  (string/split-lines)
                  parse)]
    (when (not= 119760 (part2 input))
      (println "Somethin went wrong.")
      (System/exit 1))
    (println (format "bench: %.3e" (bench #(part2 input))))
    (Thread/sleep 3000)
    (println "Waiting for profiler...")
    (read-line)
    (part2 input)))
