(ns t.day07
  (:require [t.lib :as lib :refer [l]]
            [clojure.string :as string]
            [clojure.set :as set]
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

(defn parse
  [lines]
  (->> lines
       (map parser)
       (map second)
       (map (fn [line] (if (= :file (first line))
                         (update line 1 l)
                         line)))))

(defn prefixes
  [v]
  (if (= 1 (count v))
    [v]
    (cons v (prefixes (pop v)))))

(defn part1
  [input]
  (->> input
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
       :tree
       (keep (fn [[_ size]] (when (< size 100000) size)))
       (reduce + 0)))

(defn part2
  [input]
  )

(lib/check
  parse
  part1 95437 1491614
  ;part2 4 804
  )
