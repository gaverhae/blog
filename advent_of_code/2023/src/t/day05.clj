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
                                   (fn [in]
                                     (when (< src in (+ src rng))
                                       (+ dest (- in src))))))
                            ((fn [fns]
                               (fn [in]
                                 (or (first (keep (fn [f] (f in)) fns))
                                     in))))))))}))

(defn part1
  [{:keys [seeds maps]}]
  (->> seeds
       (map (fn [seed]
              (reduce (fn [acc el] (el acc))
                      seed
                      maps)))
       (reduce min)))


(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 35
  [part1 puzzle] 240320250
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
