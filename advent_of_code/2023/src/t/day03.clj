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
  [{:keys [numbers symbols]}]
  (let [parts (->> numbers
                   (map (fn [[n [y x0]]]
                          [n (set (for [x (range x0 (+ x0 (count n)))]
                                           [y x]))])))]
    (->> symbols
         (filter (fn [[s _]] (= s \*)))
         (map (fn [[s [y x]]]
                [s (set [[(dec y) (dec x)] [(dec y) x] [(dec y) (inc x)]
                         [     y  (dec x)] [     y  x] [     y  (inc x)]
                         [(inc y) (dec x)] [(inc y) x] [(inc y) (inc x)]])]))
         (map (fn [[s adj]]
                [s (->> parts
                        (filter (fn [[n ps]] (seq (set/intersection adj ps))))
                        (map (comp ->long first)))]))
         (filter (fn [[s adj]] (= 2 (count adj))))
         (map second)
         (map (fn [[x y]] (* x y)))
         (reduce + 0))))

(lib/check
  [part1 sample] 4361
  [part1 puzzle] 559667
  [part2 sample] 467835
  [part2 puzzle] 86841457)
