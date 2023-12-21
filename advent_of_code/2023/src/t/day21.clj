(ns t.day21
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  {:grid lines
   :start (->> lines
               (map-indexed (fn [y line]
                              (->> line
                                   (keep-indexed (fn [x c]
                                                   (when (= c \S)
                                                     [y x]))))))
               (apply concat)
               first)})

(defn part1
  [input max-steps]
  (loop [step 0
         ps [(:start input)]]
    (if (= max-steps step)
      (count ps)
      (recur (inc step)
             (->> ps
                  (mapcat (fn [[y x]]
                            (for [[dy dx] [[-1 0] [1 0] [0 1] [0 -1]]
                                  :let [y (+ y dy)
                                        x (+ x dx)]
                                  :when (#{\S \.} (get-in input [:grid y x]))]
                              [y x])))
                  set)))))

(defn part2
  [input max-steps]
  (let [h (-> input :grid count)
        w (-> input :grid first count)]
    (loop [step 0
           ps #{(:start input)}]
      (println)
      (->> (range (->> ps (map first) (reduce min))
                  (->> ps (map first) (reduce max) inc))
           (map (fn [y]
                  (->> (range (->> ps (map second) (reduce min))
                              (->> ps (map second) (reduce max) inc))
                       (map (fn [x] (if (ps [y x]) \O (get-in input [:grid (mod y h) (mod x w)]))))
                       (apply str)
                       println)))
           doall)
      (println)
      (if (= max-steps step)
        (count ps)
        (recur (inc step)
               (->> ps
                    (mapcat (fn [[y x]]
                              (for [[dy dx] [[-1 0] [1 0] [0 1] [0 -1]]
                                    :let [y (+ y dy)
                                          x (+ x dx)]
                                    :when (#{\S \.} (get-in input [:grid (mod y h) (mod x w)]))]
                                [y x])))
                    set))))))

(lib/check
  [part1 sample 6] 16
  [part1 puzzle 64] 3639
  [part2 sample 6] 16
  [part2 sample 10] 50
  [part2 sample 50] 1594
  [part2 sample 100] 6536
  #_#_[part2 sample 500] 167004
  #_#_[part2 sample 1000] 668697
  #_#_[part2 sample 5000] 16733044
  #_#_[part2 puzzle 100] 0
  #_#_[part2 puzzle 26501365] 0)
