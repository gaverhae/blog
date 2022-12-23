(ns t.day22
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (let [[b [p]] (->> lines
                     (partition-by #{""})
                     (remove #{[""]}))]
    {:map-lines b
     :start [1 (inc (.indexOf ^String (first lines) "."))]
     :path (->> p
                (partition-by #(Character/isDigit ^Character %))
                (map (fn [s]
                       (if (every? #(Character/isDigit ^Character %) s)
                         (->long (apply str s))
                         (apply str s)))))}))

(defn walk-path
  [board start path]
  (loop [[y x :as pos] start
         dir (long 0)
         path path]
    (prn [pos dir])
    (cond (empty? path)
          (+ (* 1000 y)
             (* 4 x)
             dir)
          (= "R" (first path))
          (recur pos (long (mod (inc dir) 4)) (rest path))
          (= "L" (first path))
          (recur pos (long (mod (dec dir) 4)) (rest path))
          :else
          (let [[pos dir] (loop [step (first path)
                                 pos pos
                                 dir dir]
                            (prn [:step pos dir (get-in board [pos dir])])
                            (if (== 0 step)
                              [pos dir]
                              (let [[pos dir] (or (get-in board [pos dir]) [pos dir])]
                                (recur (dec step) pos dir))))]
            (recur pos dir (rest path))))))

(defn part1
  [{:keys [map-lines start path]}]
  (let [height (count map-lines)
        width (->> map-lines (map count) (apply max))
        b (->> (for [j (range height)
                     :let [s (nth map-lines j)]
                     i (range width)]
                 [[j i] (get s i \space)])
               (into {}))
        board (->> b
                   (keep (fn [[[y x] c]]
                           (when (not= \space c)
                             [[(inc y) (inc x)]
                              [(loop [x (mod (inc x) width)]
                                 (case (b [y x])
                                   \space (recur (mod (inc x) width))
                                   \. [[(inc y) (inc x)] 0]
                                   \# nil))
                               (loop [y (mod (inc y) height)]
                                 (case (b [y x])
                                   \space (recur (mod (inc y) height))
                                   \. [[(inc y) (inc x)] 1]
                                   \# nil))
                               (loop [x (mod (dec x) width)]
                                 (case (b [y x])
                                   \space (recur (mod (dec x) width))
                                   \. [[(inc y) (inc x)] 2]
                                   \# nil))
                               (loop [y (mod (dec y) height)]
                                 (case (b [y x])
                                   \space (recur (mod (dec y) height))
                                   \. [[(inc y) (inc x)] 3]
                                   \# nil))]])))
                   (into {}))]
    (walk-path board start path)))

(defn part2
  [{:keys [map-lines start path]} cube-size]
  (let [height (count map-lines)
        width (->> map-lines (map count) (apply max))
        b (->> (for [j (range height)
                     :let [s (nth map-lines j)]
                     i (range width)]
                 [[j i] (get s i \space)])
               (into {}))
        board (->> b
                   (keep (fn [[[y x] c]]
                           (when (not= \space c)
                             [[y x]
                              [(cond (and (<= (inc x) (dec width))
                                          (not= \space (get b [y (inc x)])))
                                     [[y (inc x)] 0]
                                     (= 4 cube-size)
                                     (cond (<= 0 y 3)
                                           [[(- 11 y) 15] 2]
                                           (<= 4 y 7)
                                           [[8 (- 19 y)] 1]
                                           (<= 8 y 11)
                                           [[(- 11 y) 11] 2])
                                     (= 50 cube-size)
                                     (cond (<= 0 y 49)
                                           [[(- 149 y) 99] 2]
                                           (<= 50 y 99)
                                           [[49 (+ y 50)] 3]
                                           (<= 100 y 149)
                                           [[(- 149 y) 149] 2]
                                           (<= 150 y 199)
                                           [[149 (- y 100)] 3]))
                              (cond (and (<= (inc y) (dec height))
                                         (not= \space (get b [(inc y) x])))
                                    [[(inc y) x] 1]
                                    (= 4 cube-size)
                                    (cond (<= 0 x 3)
                                          [[11 (- 11 x)] 3]
                                          (<= 4 x 7)
                                          [[(- 15 x) 8] 0]
                                          (<= 8 x 11)
                                          [[7 (- 11 x)] 3]
                                          (<= 12 x 15)
                                          [[(- 19 x) 0] 0])
                                    (= 50 cube-size)
                                    (cond (<= 0 x 49)
                                          [[0 (+ 100 x)] 1]
                                          (<= 50 x 99)
                                          [[(+ 100 x) 49] 2]
                                          (<= 100 x 149)
                                          [[(- x 50) 99] 2]))
                              (cond (and (<= 0 (dec x))
                                         (not= \space (get b [y (dec x)])))
                                    [[y (dec x)] 2]
                                    (= 4 cube-size)
                                    (cond (<= 0 y 3)
                                          [[4 (+ 4 y)] 1]
                                          (<= 4 y 7)
                                          [[11 (- 19 y)] 3]
                                          (<= 8 y 11)
                                          [[7 (- 15 y)] 3])
                                    (= 50 cube-size)
                                    (cond (<= 0 y 49)
                                          [[(- 149 y) 0] 0]
                                          (<= 50 y 99)
                                          [[100 (- y 50)] 1]
                                          (<= 100 y 149)
                                          [[(- 149 y) 50] 0]
                                          (<= 150 y 199)
                                          [[0 (- y 100)] 1]))
                              (cond (and (<= 0 (dec y))
                                         (not= \space (get b [(dec y) x])))
                                    [[(dec y) x] 3]
                                    (= 4 cube-size)
                                    (cond (<= 0 x 3)
                                          [[0 (- 11 x)] 1]
                                          (<= 4 x 7)
                                          [[(- y 4) x] 0]
                                          (<= 8 x 11)
                                          [[4 (- 11 x)] 1]
                                          (<= 12 x 15)
                                          [[(- 19 x) 11] 2])
                                    (= 50 cube-size)
                                    (cond (<= 0 x 49)
                                          [[(+ 50 x) 50] 0]
                                          (<= 50 x 99)
                                          [[(+ 100 x) 0] 0]
                                          (<= 100 x 149)
                                          [[199 (- x 100)] 3]))]])))
                   (map (fn [[[y x] [& steps]]]
                          (prn [:pos [y x] :steps steps])
                          [[(inc y) (inc x)] (->> steps
                                                  (mapv (fn [[[y x] dir]]
                                                          (when (= \. (get b [y x]))
                                                            [[(inc y) (inc x)] dir]))))]))
                   (into {}))]
    (->> board sort (mapv prn))
    (walk-path board start path)))

(lib/check
  #_#_[part1 sample] 6032
  #_#_[part1 puzzle] 117102
  #_#_[part2 sample 4] 5031
  #_#_[part2 puzzle 50] 0)
