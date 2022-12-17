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
        (print (case (get tower [y x])
                 :floor \-
                 :rock \#
                 nil \.)))
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
  [piece tower]
  (reduce (fn [t c] (assoc t c :rock)) tower piece))

(defn part1
  [input]
  (loop [i 0
         pieces (cycle (:pieces input))
         jets (cycle (:jets input))
         tower {[0 0] :floor, [0 1] :floor, [0 2] :floor, [0 3] :floor,
                [0 4] :floor, [0 5] :floor, [0 6] :floor}]
    #_(println i)
    #_(print-tower tower)
    (if (== i 2022)
      (height tower)
      (let [[jets tower]
            (loop [jets jets
                   piece (make-piece (first pieces) (height tower))]
              (let [p1 (push piece (first jets) tower)
                    p2 (fall p1 tower)]
                #_(prn [:piece piece :direction (first jets) :after p1])
                (if (= p1 p2)
                  [(rest jets) (stop p2 tower)]
                  (recur (rest jets) p2))))]
        (recur (inc i) (rest pieces) jets tower)))))

(defn part2
  [input]
  (take 5 input))

(lib/check
  [part1 sample] 3068
  [part1 puzzle] 3151
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0
  )

