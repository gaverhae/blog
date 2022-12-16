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
  (loop [i 0
         states {["AA" "AA"] {#{} 0}}]
    (prn [i (->> states vals (mapcat vals) count)])
    (if (== 26 i)
      (->> states vals (mapcat vals) sort reverse first)
      (recur (inc i)
             (->> states
                  (mapcat (fn [[[cur-h cur-e] states]]
                            (->> states
                                 (mapcat (fn [[opened? released]]
                                           (let [r (reduce +
                                                           released
                                                           (map #(get-in input [% :rate])
                                                                opened?))]
                                             (for [h (conj (get-in input [cur-h :tunnels]) cur-h)
                                                   e (conj (get-in input [cur-e :tunnels]) cur-e)
                                                   :when (or (and (not= h cur-h)
                                                                  (not= e cur-e))
                                                             (and (not= h cur-h)
                                                                  (not (opened? cur-e))
                                                                  (pos? (get-in input [cur-e :rate])))
                                                             (and (not= e cur-e)
                                                                  (not (opened? cur-h))
                                                                  (pos? (get-in input [cur-h :rate])))
                                                             (and (not (opened? cur-e))
                                                                  (pos? (get-in input [cur-e :rate]))
                                                                  (not (opened? cur-h))
                                                                  (pos? (get-in input [cur-h :rate]))))]
                                               [(vec (sort [h e]))
                                                [(cond-> opened?
                                                   (= h cur-h) (conj h)
                                                   (= e cur-e) (conj e))
                                                 r]])))))))
                  (reduce (fn [states [pos [opened released]]]
                            (update-in states [pos opened] (fnil max 0) released))
                          {}))))))

(lib/check
  #_#_[part1 sample] 1651
  #_#_[part1 puzzle] 1871
  [part2 sample] 1707
  [part2 puzzle] 0
  )
