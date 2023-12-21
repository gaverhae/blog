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
        w (-> input :grid first count)
        input (update input :grid (fn [g] (->> g
                                               (map (fn [line] (str line line line)))
                                               (repeat 3)
                                               (apply concat)
                                               vec)))
        start (let [[y x] (-> input :start)]
                [(+ y h) (+ x w)])]
    (->> input :grid (map prn) doall)
    (loop [step 0
           ps #{start}
           past-states #{}]
      #_(prn [(count past-states) (map count past-states)])
      (prn (count ps))
      (if (past-states ps)
        (count ps)
        (recur (inc step)
               (->> ps
                    (mapcat (fn [[y x]]
                              (for [[dy dx] [[-1 0] [1 0] [0 1] [0 -1]]
                                    :let [y (+ y dy)
                                          x (+ x dx)]
                                    :when (#{\S \.} (get-in input [:grid y x]))]
                                [y x])))
                    set)
               (conj past-states ps))))))

(lib/check
  [part1 sample 6] 16
  [part1 puzzle 64] 3639
  [part2 sample 6] 16
  [part2 sample 10] 50
  [part2 sample 50] 1594
  #_#_[part2 sample 100] 6536
  #_#_[part2 sample 500] 167004
  #_#_[part2 sample 1000] 668697
  #_#_[part2 sample 5000] 16733044
  #_#_[part2 puzzle 100] 0
  #_#_[part2 puzzle 26501365] 0)
