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
        ->neighs (->> (range h)
                      (mapcat (fn [y]
                                (->> (range w)
                                     (map (fn [x]
                                            [[y x] (for [[dy dx] [[-1 0] [1 0] [0 1] [0 -1]]
                                                         :let [y (+ y dy)
                                                               x (+ x dx)]
                                                         :when (#{\S \.} (get-in input [:grid (mod y h) (mod x w)]))]
                                                     [dy dx])])))))
                      (into {}))]
    (loop [step 0
           cur #{(:start input)}
           prev #{}
           internal-points {:on #{}, :off #{}}
           internal [0 0]]
      (if (= max-steps step)
        (+ (count cur) (get internal (rem step 2)))
        (let [ip {:on (:off internal-points)
                  :off (set/union (:on internal-points)
                                  cur)}
              nxt (->> cur
                       (mapcat (fn [[y x]]
                                 (->> (->neighs [(mod y h) (mod x w)])
                                      (map (fn [[dy dx]] [(+ y dy) (+ x dx)])))))
                       (remove prev)
                       set)]
          (recur (inc step)
                 nxt
                 cur
                 ip
                 (update internal (rem step 2) + (count cur))))))))

(lib/check
  #_#_[part1 sample 6] 16
  #_#_[part1 puzzle 64] 3639
  [part2 sample 6] 16
  [part2 sample 10] 50
  [part2 sample 50] 1594
  [part2 sample 1] 2
  [part2 sample 10] 50
  [part2 sample 100] 6536
  [part2 sample 200] 26538
  [part2 sample 300] 59895
  [part2 sample 400] 106776
  [part2 sample 500] 167004
  #_#_[part2 sample 1000] 668697
  #_#_[part2 sample 2000] 2677337
  #_#_[part2 sample 5000] 16733044
  [part2 puzzle 100] 8829
  [part2 puzzle 200] 34889
  [part2 puzzle 400] 138314
  #_#_[part2 puzzle 1000] 862969
  #_#_[part2 puzzle 2000] 862969
  #_#_[part2 puzzle 26501365] 0)

(defn benchmark
  []
  (doseq [data [#'sample #'puzzle]
          n [100 300 1000 3000]]
    (let [start (System/currentTimeMillis)
          _ (part2 @@data n)
          end (System/currentTimeMillis)]
      (println (format "%10s %4s: %6.2fs"
                       (-> data meta :name)
                       n
                       (/ (- end start) 1000.0))))))
