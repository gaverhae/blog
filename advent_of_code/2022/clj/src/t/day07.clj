(ns t.day07
  (:require [t.lib :as lib :refer [l]]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]))

(def parser
  (insta/parser
    "S = cd | ls | dir | file
    cd = <'$ cd '> word
    ls = <'$ ls'>
    dir = <'dir '> word
    file = number <' '> word
    <number> = #'\\d+'
    <word> = #'[a-zA-Z0-9./]+'"))

(defn prefixes
  [v]
  (if (= 1 (count v))
    [v]
    (cons v (prefixes (pop v)))))

(defn parse
  [lines]
  (->> lines
       (map parser)
       (map second)
       (map (fn [line] (if (= :file (first line))
                         (update line 1 l)
                         line)))
       (reduce (fn [acc el]
                 (match el
                   [:cd "/"] (assoc acc :cwd ["/"])
                   [:cd ".."] (update acc :cwd pop)
                   [:cd child] (update acc :cwd conj child)
                   [:ls] acc
                   [:dir d] acc
                   [:file size _] (assoc acc :tree
                                         (reduce
                                           (fn [acc el]
                                             (update acc el (fnil + 0) size))
                                           (:tree acc)
                                           (prefixes (:cwd acc))))))
               {})
       :tree))

(defn part1
  [input]
  (->> input
       (keep (fn [[_ size]] (when (< size 100000) size)))
       (reduce + 0)))

(defn part2
  [input]
  (let [required-space (- 30000000 (- 70000000 (get input ["/"])))]
    (->> input
         (keep (fn [[_ size]] (when (>= size required-space) size)))
         sort
         first)))

(lib/check
  parse
  part1 95437 1491614
  part2 24933642 6400111)
