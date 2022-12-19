(ns t.day19
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
              (let [[_ bp ore-ore clay-ore obsidian-ore obsidian-clay geode-ore geode-obsidian]
                    (re-matches
                      #"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
                      line)]
                {:id (->long bp)
                 :blueprints {:ore {:ore (->long ore-ore)}
                              :clay {:ore (->long clay-ore)}
                              :obsidian {:ore (->long obsidian-ore)
                                         :clay (->long obsidian-clay)}
                              :geode {:ore (->long geode-ore)
                                      :obsidian (->long geode-obsidian)}}})))))

(defn possible-moves
  [bp]
  (fn [state]
    (let [build (for [[robot cost] (-> bp :blueprints)
                      :when (every? (fn [[k v]]
                                      (>= (get-in state [:inventory k] 0) v))
                                    cost)]
                  (-> state
                      (update :inventory (fn [inv] (reduce (fn [inv [k v]] (update inv k - v))
                                                           inv
                                                           cost)))
                      (update-in [:robots robot] (fnil inc 0))))]
      (->> (conj build state)
           (mapv (fn [s] (update s :inventory (fn [inv]
                                               (reduce (fn [inv [k v]] (update inv k (fnil + 0) v))
                                                       inv
                                                       (:robots state))))))))))

(defn part1
  [input]
  (->> input
       (map (fn [bp]
              (loop [t 0
                     states #{{:robots {:ore 1} :inventory {}}}]
                (prn [(:id bp) t (count states)])
                (cond (== t 24)
                      (->> states
                           (map (fn [s] (-> s :inventory (:geode 0))))
                           (reduce max)
                           (* (:id bp)))
                      (>= t 18)
                      (recur (inc t)
                             (->> states
                                  (mapcat (possible-moves bp))
                                  (filter (fn [s]
                                            (> (get-in s [:inventory :geode] 0) 0)))
                                  set))
                      :else
                      (recur (inc t)
                             (->> states
                                  (mapcat (possible-moves bp))
                                  set))))))
       (reduce +)))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 33
  [part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0
  )

