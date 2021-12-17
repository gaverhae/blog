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

(defn traverse
  [input init forbidden update-state]
  (loop [[paths num-paths] [[init] 0]]
    (if (empty? paths)
      num-paths
      (recur (reduce (fn [[ps np] [pos state]]
                       (reduce (fn [[ps np] next-step]
                                 (cond (= "end" next-step) [ps (inc np)]
                                       (forbidden state next-step) [ps np]
                                       :else [(conj ps [next-step
                                                        (update-state state next-step)])
                                              np]))
                               [ps np]
                               (get input pos)))
                     [[] num-paths]
                     paths)))))

(defn part1
  [input]
  (traverse input
            ["start" #{"start"}]
            contains?
            (fn [visited cave]
              (if (small? cave)
                (conj visited cave)
                visited))))

(defn part2
  [input]
  (traverse input
            ["start" [#{"start"} false]]
            (fn [[visited twice?] cave]
              (or (= "start" cave)
                  (and (visited cave) twice?)))
            (fn [[visited twice?] cave]
              [(if (small? cave)
                 (conj visited cave)
                 visited)
               (or twice?
                   (boolean (visited cave)))])))
