(ns t.day14
  (:require [clojure.core.async :as async]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  lines)

(defn part1
  [input]
  (->> (cons (repeat (count (first input)) \#)
             input)
       lib/transpose
       (map (fn [col]
              (->> (apply str col)
                   (re-seq #"#+[.O]*")
                   (mapcat (fn [s]
                             (let [f (frequencies s)]
                               (concat (repeat (f \#) \#)
                                       (repeat (f \O 0) \O)
                                       (repeat (f \. 0) \.)))))
                   reverse
                   (apply str))))
       (map (fn [line]
              (loop [idx 0
                     ld 0]
                (if (== idx (count line))
                  ld
                  (recur (inc idx)
                         (+ (if (= \O (get line idx)) (inc idx) 0)
                            ld))))))
       (reduce + 0)))


(defn part2
  [input]
  (let [tilt (fn [grid]
               (->> grid
                    lib/transpose
                    (map (fn [col]
                           (->> (apply str col)
                                (re-seq #"#+[.O]*")
                                (mapcat (fn [s]
                                          (let [f (frequencies s)]
                                            (concat (repeat (f \#) \#)
                                                    (repeat (f \O 0) \O)
                                                    (repeat (f \. 0) \.)))))
                                (apply str))))
                    lib/transpose))
        rotate (fn [grid]
                 (->> grid
                      lib/transpose
                      (map (fn [line]
                             (->> line reverse (apply str))))))
        apply-cycle (memoize (fn [grid]
                               (->> grid
                                    tilt
                                    rotate
                                    tilt
                                    rotate
                                    tilt
                                    rotate
                                    tilt
                                    rotate)))
        score (fn [grid]
                (->> grid
                     lib/transpose
                     (map (fn [col] (apply str (reverse col))))
                     (map (fn [line]
                            (loop [idx 0
                                   ld 0]
                              (if (== idx (count line))
                                ld
                                (recur (inc idx)
                                       (+ (if (= \O (get line idx)) idx 0)
                                          ld))))))
                     (reduce + 0)))
        init (concat [(apply str (repeat (+ 2 (count (first input))) \#))]
                     (->> input (map (fn [l] (str \# l \#))))
                     [(apply str (repeat (+ 2 (count (first input))) \#))])
        [first-in-cycle cycle-length scores]
        (loop [idx 0
               cur init
               grid->num {cur 0}
               num->score {0 (score init)}]
          (let [nxt (apply-cycle cur)
                idx (inc idx)]
            (if-let [n (grid->num nxt)]
              [n (- idx n) num->score]
              (recur idx nxt (assoc grid->num nxt idx) (assoc num->score idx (score nxt))))))
        response-index (+ first-in-cycle
                          (rem (- 1000000000 first-in-cycle)
                               cycle-length))]
    (scores response-index)))

(lib/check
  [part1 sample] 136
  [part1 puzzle] 109638
  [part2 sample] 64
  [part2 puzzle] 102657)
