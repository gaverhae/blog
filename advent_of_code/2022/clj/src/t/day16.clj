(ns t.day16
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [[_ valve rate tunnels]
                    (re-matches #"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.*)" line)]
                [valve {:rate (->long rate)
                        :tunnels (string/split tunnels #", ")}])))
       (into {})))

(defn part1
  [input]
  (loop [i 0
         states {"AA" {#{} 0}}]
    (prn [i (->> states vals (mapcat vals) count)])
    (if (== 30 i)
      (->> states vals (mapcat vals) sort reverse first)
      (recur (inc i)
             (->> states
                  (mapcat (fn [[cur-pos states]]
                            (->> states
                                 (mapcat (fn [[opened? released]]
                                           (let [r (reduce +
                                                           released
                                                           (map #(get-in input [% :rate])
                                                                opened?))]
                                             (->> (get-in input [cur-pos :tunnels])
                                                  (map (fn [n] [n [opened? r]]))
                                                  (concat (when (and (not (opened? cur-pos))
                                                                     (pos? (get-in input [cur-pos :rate])))
                                                            [[cur-pos [(conj opened? cur-pos) r]]])))))))))
                  (reduce (fn [states [pos [opened released]]]
                            (update-in states [pos opened] (fnil max 0) released))
                          {}))))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 1651
  [part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0
  )
