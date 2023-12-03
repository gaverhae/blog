(ns t.day03
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (map-indexed
         (fn [y line]
           (loop [p 0
                  numbers []
                  symbols []]
             (let [c ^char (get line p)]
               (cond (== p (count line))
                     [numbers symbols]
                     (= \. c)
                     (recur (inc p) numbers symbols)
                     (Character/isDigit c)
                     (let [n (apply str (take-while #(Character/isDigit ^char %) (subs line p)))]
                       (recur (+ p (count n))
                              (conj numbers [n [y p]])
                              symbols))
                     :else
                     (recur (inc p)
                            numbers
                            (conj symbols [c [y p]])))))))
       (reduce (fn [acc [numbers symbols]]
                 (-> acc
                     (update :numbers (fnil concat []) numbers)
                     (update :symbols (fnil concat []) symbols)))
               {})))

(defn part1
  [{:keys [symbols numbers]}]
  (let [adj (->> symbols
                 (map (fn [[s [y x]]]
                        [[s y x] (set [[(dec y) (dec x)] [(dec y) x] [(dec y) (inc x)]
                                       [     y  (dec x)] [     y  x] [     y  (inc x)]
                                       [(inc y) (dec x)] [(inc y) x] [(inc y) (inc x)]])])))]
    (->> numbers
         (filter (fn [[n [y x0]]]
                   (let [ps (set (for [x (range x0 (+ x0 (count n)))] [y x]))]
                     (->> adj
                          (filter (fn [[s as]] (seq (set/intersection as ps))))
                          (map first)
                          seq))))
         (map (comp ->long first))
         (reduce + 0))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 4361
  [part1 puzzle] 559667
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
