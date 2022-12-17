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

(defn tower->line
  [tower y]
  (apply str (for [x (range 7)]
               (if-let [d (tower [y x])]
                 (str (mod d 5))
                 "."))))

(defn print-tower
  [tower]
  (println)
  (let [h (height tower)]
    (loop [y 0]
      (when (<= y h)
        (doseq [x (range 7)]
          (print (tower [y x] \.)))
        (println) (flush)
        (recur (inc y))))))

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

(defn step
  [[tower pieces jets] i]
  (loop [jets jets
         piece (make-piece (first pieces) (height tower))]
    (let [p1 (push piece (first jets) tower)
          p2 (fall p1 tower)]
      (if (= p1 p2)
        [(stop p2 i tower) (rest pieces) (rest jets)]
        (recur (rest jets) p2)))))

(defn init-state
  [input]
  [{[0 0] 0, [0 1] 0, [0 2] 0, [0 3] 0,
    [0 4] 0, [0 5] 0, [0 6] 0}
   (cycle (:pieces input))
   (cycle (:jets input))])

(defn part1
  [input]
  (height (first (reduce step (init-state input) (range 2022)))))

(defn ->lines
  [tower]
  (let [h (height tower)]
    (for [y (range (inc h))]
      (tower->line tower y))))

(defn part2
  [input]
  (let [num-pieces 1000000000000
        init-state [{[0 0] 0, [0 1] 0, [0 2] 0, [0 3] 0,
                     [0 4] 0, [0 5] 0, [0 6] 0}
                    (cycle (:pieces input))
                    (cycle (:jets input))]
        [tower pieces jets] (reduce step init-state (range 4000))
        h (height tower)
        t (->> tower
               ->lines
               (partition 10 1)
               vec)
        pat (get t (- (count t) 10))
        beg (.indexOf ^java.util.List t pat)
        period-length (inc (.indexOf ^java.util.List (vec (drop (inc beg) t)) pat))
        pieces-in-period (->> tower
                              (keep (fn [[[y x] p]]
                                      (when (<= beg y (+ beg period-length))
                                        p)))
                              set count dec)
        periods-to-skip (quot (- num-pieces 4000) pieces-in-period)
        height-to-add (* periods-to-skip period-length)
        remaining-pieces (rem (- num-pieces 4000) pieces-in-period)
        [tower' _ _] (reduce step [tower pieces jets] (range remaining-pieces))]
    (+ height-to-add
       (height tower'))))

(lib/check
  #_#_[part1 sample] 3068
  #_#_[part1 puzzle] 3151
  [part2 sample] 1514285714288
  [part2 puzzle] 0
  )

