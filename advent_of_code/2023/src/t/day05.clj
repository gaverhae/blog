(ns t.day05
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib]))

(def parser
  (insta/parser
    "<S> = seeds map+
     seeds = <'seeds: '> nums
     nums = num (<' '> num)+
     <num> = #'\\d+'
     nl = #'\\n'
     map = <nl> <nl> <map-title> <nl> nums (<nl> nums)+
     map-title = #'[a-z -]+:'
    "))

(defn parse
  [lines]
  (let [[[_ [_ & seeds]] & maps] (parser (apply str (interpose "\n" lines)))]
    {:seeds (->> seeds (map parse-long))
     :maps (->> maps
                (map (fn [[_ & nums]]
                       (->> nums
                            (map (fn [[_ & nums]]
                                   (map parse-long nums)))
                            (map (fn [[dest src rng]]
                                   [src (dec (+ src rng)) (- dest src)]))
                            ((fn [ranges]
                               (let [ranges (sort ranges)]
                                 (fn [ins]
                                   (prn [:ins ins])
                                   (loop [to-process ins
                                          to-match ranges
                                          done []]
                                     (if (or (empty? to-process)
                                             (empty? to-match))
                                       (sort (set (concat to-process done)))
                                       (let [[p0 p1] (first to-process)
                                             [m0 m1 offset] (first to-match)]
                                         (cond (< p1 m0) (recur (rest to-process)
                                                                to-match
                                                                (cons [p0 p1] done))
                                               (< m1 p0) (recur to-process
                                                                (rest to-match)
                                                                done)
                                               (and (<= m0 p0) (< m1 p1)) (recur (cons [(inc m1) p1] to-process)
                                                                                 (rest to-match)
                                                                                 (cons [(+ offset p0) (+ offset m1)] done))
                                               (and (<= m0 p0) (<= p1 m1)) (recur (rest to-process)
                                                                                  to-match
                                                                                  (cons [(+ offset p0) (+ offset p1)] done))
                                               (and (< p0 m0) (< m1 p1)) (recur (cons [(inc m1) p1] to-process)
                                                                                (rest to-match)
                                                                                (concat [[p0 (dec m0)] [(+ offset m0) (+ offset m1)]]
                                                                                        done))
                                               (and (< p0 m0) (<= p1 m1)) (recur (rest to-process)
                                                                                 to-match
                                                                                 (concat [[p0 (dec m0)] [(+ offset m0) (+ offset p1)]]
                                                                                         done))))))))))))))}))

(defn part1
  [{:keys [seeds maps]}]
  (->> seeds
       (mapcat (fn [seed]
                 (reduce (fn [acc el] (el acc))
                         [[seed seed]]
                         maps)))
       (map first)
       (reduce min)))



(defn part2
  [{:keys [seeds maps]}]
  (->> seeds
       (partition 2)
       (map (fn [[a b]] [a (+ a b)]))
       ((fn [seed-ranges]
          (reduce (fn [acc el] (el acc)) seed-ranges maps)))
       #_(map first)
       #_(reduce min)))

(lib/check
  #_#_[part1 sample] 35
  #_#_[part1 puzzle] 240320250
  #_#_[part2 sample] 46
  [part2 puzzle] -10)
