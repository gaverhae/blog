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
         states #{{:pos "AA", :opened? #{}, :released 0}}]
    (prn [i (count states)])
    (if (== 30 i)
      (->> states (map :released) sort reverse first)
      (recur (inc i)
             (->> states
                  (mapcat (fn [s]
                            (let [r (reduce +
                                            (:released s)
                                            (map #(get-in input [% :rate])
                                                 (:opened? s)))]
                              (->> (get-in input [(:pos s) :tunnels])
                                   (map (fn [n]
                                          (-> s
                                              (assoc :pos n)
                                              (assoc :released r))))
                                   (concat (when (and (not ((:opened? s) (:pos s)))
                                                      (pos? (get-in input [(:pos s) :rate])))
                                             [(-> s
                                                  (update :opened? conj (:pos s))
                                                  (assoc :released r))]))))))
                  set)))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 1651
  [part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0
  )
