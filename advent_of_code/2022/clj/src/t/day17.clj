(ns t.day17
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  {:pieces [[[0 0] [0 1] [0 2] [0 3]]
            [      [2 1]
             [1 0] [1 1] [1 2]
                   [0 1]]
            [            [2 2]
                         [1 2]
             [0 0] [0 1] [0 2]]
            [[3 0]
             [2 0]
             [1 0]
             [0 0]]
            [[1 0] [1 1]
             [0 0] [0 1]]]
   :jets (->> lines
              first
              (map {\< -1, \> 1}))})

(defn height
  [tower]
  (->> tower keys (map first) (reduce max)))

(defn make-piece
  [p h]
  (->> p
       (mapv (fn [[y x]]
               [(+ y h 4) (+ x 2)]))))

(defn print-tower
  [tower]
  (println)
  (loop [y 0]
    (when (<= y (height tower))
      (doseq [x (range 7)]
        (print (tower [y x] \.)))
      (println) (flush)
      (recur (inc y)))))

(defn push
  [piece direction tower]
  (let [[m c x] ({1 [max < 6], -1 [min > 0]} direction)
        p (->> piece (map second) (reduce m))
        pushed (->> piece (map (fn [[y x]] [y (+ x direction)])))]
    (if (and (c p x)
             (->> pushed (keep tower) empty?))
      pushed
      piece)))

(defn fall
  [piece tower]
  (let [fallen (->> piece (map (fn [[y x]] [(dec y) x])))]
    (if (->> fallen (keep tower) empty?)
      fallen
      piece)))

(defn stop
  [piece n tower]
  (reduce (fn [t c] (assoc t c n)) tower piece))

(def prev-height (atom 0))

(defn part1
  [input]
  (reset! prev-height 0)
  (loop [i 0
         pieces (cycle (map-indexed vector (:pieces input)))
         jets (cycle (:jets input))
         tower {[0 0] 0, [0 1] 0, [0 2] 0, [0 3] 0,
                [0 4] 0, [0 5] 0, [0 6] 0}]
    (when (== 0 (mod i 5))
      (println [i (- (height tower) @prev-height)])
      (reset! prev-height (height tower)))
    #_(println i)
    #_(print-tower tower)
    (if (== i 2022)
      (do #_(print-tower tower)
        (height tower))
      (let [[p piece] (first pieces)
            [jets tower]
            (loop [jets jets
                   piece (make-piece piece (height tower))]
              (let [p1 (push piece (first jets) tower)
                    p2 (fall p1 tower)]
                #_(prn [:piece piece :direction (first jets) :after p1])
                (if (= p1 p2)
                  [(rest jets) (stop p2 p tower)]
                  (recur (rest jets) p2))))]
        (recur (inc i) (rest pieces) jets tower)))))

(defn part2
  [input]
  (count (:jets input)))

(lib/check
  #_#_[part1 sample] 3068
  [part1 puzzle] 3151
  [part2 sample] 0
  [part2 puzzle] 0
  )

