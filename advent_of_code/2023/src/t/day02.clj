(ns t.day02
  (:require [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))
(def parser
  (insta/parser
    "<S> = <'Game '> num <': '> drawings+
    <drawings> = drawing (<'; '> drawing)+
    drawing = show (<', '> show)*
    show = num <' '> color
    <num> = #'\\d+'
    <color> = 'red' | 'green' | 'blue'"))

(defn parse
  [lines]
  (->> (map parser lines)
       (map (fn [[id & drawings]]
              {:id (->long id)
               :drawings (->> drawings
                              (map (fn [[_ & shows]]
                                     (map (fn [[_ n color]]
                                            [(keyword color) (->long n)])
                                          shows))))}))
       (map (fn [{:keys [id drawings]}]
              [id (->> drawings
                       (apply concat)
                       (reduce (fn [acc [c n]]
                                 (update acc c (fnil max 0) n))
                               {}))]))))

(defn part1
  [input]
  (->> input
       (filter (fn [[id {:keys [blue green red]}]]
                 (and (<= red 12)
                      (<= green 13)
                      (<= blue 14))))
       (map first)
       (reduce + 0)))

(defn part2
  [input]
  (->> input
       (map (fn [[id {:keys [blue green red]}]]
              (* blue green red)))
       (reduce + 0)))

(lib/check
  [part1 sample] 8
  [part1 puzzle] 2593
  [part2 sample] 2286
  [part2 puzzle] 0)
