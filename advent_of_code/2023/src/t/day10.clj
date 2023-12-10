(ns t.day10
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(defn parse
  [lines]
  (vec lines))

(defn first-connected-neighbour
  [[y0 x0] grid seen?]
  (first (for [[y x c1 c2] [
                            [(inc y0) x0 #{\| \7 \F \S} #{\| \L \J \S}]
                            [(dec y0) x0 #{\| \L \J \S} #{\| \7 \F \S}]
                            [y0  (dec x0) #{\- \J \7 \S} #{\- \L \F \S}]
                            [y0  (inc x0) #{\- \F \L \S} #{\- \J \7 \S}]
                            ]
               :let [pipe (get-in grid [y x])]
               :when (and pipe
                          (c1 (get-in grid [y0 x0]))
                          (c2 pipe)
                          (not (seen? [y x])))]
           [y x])))

(defn part1
  [input]
  (let [start (->> input
                   (keep-indexed (fn [y line]
                                   (->> line (keep-indexed (fn [x c] (when (= \S c) [y x]))))))
                   (apply concat)
                   first)]
    (loop [step 0
           so-far #{}
           pos start]
      (if (and (pos? step) (= start pos))
        (quot step 2)
        (let [pos (first-connected-neighbour pos input so-far)]
          (recur (inc step)
                 (conj so-far pos)
                 pos))))))

(defn part2
  [input]
  (let [start (->> input
                   (keep-indexed (fn [y line]
                                   (->> line (keep-indexed (fn [x c] (when (= \S c) [y x]))))))
                   (apply concat)
                   first)
        loop-tile?  (loop [step 0
                           so-far #{}
                           pos start]
                      (if (and (pos? step) (= start pos))
                        so-far
                        (let [pos (first-connected-neighbour pos input so-far)]
                          (recur (inc step)
                                 (conj so-far pos)
                                 pos))))
        clean-up (fn [grid]
                   (->> grid
                        (map-indexed (fn [y line]
                                       (->> line
                                            (map-indexed (fn [x c] (if (loop-tile? [y x]) c \.)))
                                            (apply str))))
                        vec))
        replace-s (fn [grid]
                    (let [[y0 x0] start
                          up (get-in grid [(dec y0) x0])
                          right (get-in grid [y0 (inc x0)])
                          down (get-in grid [(inc y0) x0])
                          left (get-in grid [y0 (dec x0)])
                          replacement (or (and (#{\| \F \7} up) (#{\- \7 \J} right) \L)
                                          (and (#{\| \F \7} up) (#{\| \J \L} down) \|)
                                          (and (#{\| \F \7} up) (#{\- \L \F} left) \J)
                                          (and (#{\- \7 \J} right) (#{\| \L \J} down) \F)
                                          (and (#{\- \7 \J} right) (#{\- \L \F} left) \-)
                                          (and (#{\| \J \L} down) (#{\- \L \F} left) \7))]
                      (->> grid
                           (map-indexed (fn [y line]
                                          (->> line
                                               (map-indexed (fn [x c] (if (= start [y x]) replacement c)))
                                               (apply str))))
                           vec)))]
    (->> input
         clean-up
         replace-s
         (map-indexed (fn [y line]
                        (->> line
                             (map-indexed (fn [x c]
                                            (if (loop-tile? [y x])
                                              c
                                              \.)))
                             (apply str)
                             (re-seq #"\.+|\||F-*7|F-*J|L-*J|L-*7")
                             (reduce (fn [[in? so-far] el]
                                       (condp re-find el
                                         #"\.+" [in? (if in? (+ so-far (count el)) so-far)]
                                         #"\|" [(not in?) so-far]
                                         #"F-*7" [in? so-far]
                                         #"F-*J" [(not in?) so-far]
                                         #"L-*J" [in? so-far]
                                         #"L-*7" [(not in?) so-far]))
                                     [false 0])
                             second)))
         (reduce + 0))))

(lib/check
  [part1 sample] 8
  [part1 puzzle] 6968
  [part2 sample1] 4
  [part2 sample2] 10
  [part2 puzzle] 413)
