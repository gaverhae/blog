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

(defn potential
  [steps-left]
  (fn [state]
    (+ (get-in state [:inventory :geode] 0)
       (+ (* steps-left (get-in state [:robots :geode] 0))
          (reduce + (range 1 (inc steps-left)))))))

(defn part1
  [input]
  (let [n-max 24]
    (->> input
         (map (fn [bp]
                (loop [t 0
                       states #{{:robots {:ore 1} :inventory {}}}]
                  (let [
                        guaranteed-min (->> states
                                            (map (fn [state]
                                                   (+ (get-in state [:inventory :geode] 0)
                                                      (* (- n-max t) (get-in state [:robots :geode] 0)))))
                                            (reduce max 0))
                        states2 (filter (fn [state]
                                          (>= ((potential (- n-max t)) state)
                                              guaranteed-min))
                                        states)
                        f (fn [m]
                            (->> ((juxt :ore :clay :obsidian :geode) m)
                                 (mapv (fn [n] (or n 0)))))
                        mins (->> states2
                                  (reduce (fn [acc {:keys [robots inventory]}]
                                            (let [robs (f robots)
                                                  invs (f inventory)
                                                  prev (get acc robs)]
                                              (if (or (nil? prev)
                                                      (every? (fn [[a b]] (>= a b))
                                                              (map vector invs prev)))
                                                (assoc acc robs invs)
                                                acc)))
                                          {}))
                        states3 (->> states2
                                     (remove (fn [s]
                                               (let [robs (f (:robots s))
                                                     prev (get mins robs)
                                                     invs (f (:inventory s))]
                                                 (and (not (= invs prev))
                                                      (every? (fn [[a b]] (<= a b))
                                                              (map vector invs prev)))))))
                        ]
                    (prn [(:id bp) t
                          (count states)
                          (reduce max 0 (map (potential (- n-max t)) states))
                          (reduce min (map (potential (- n-max t)) states))
                          guaranteed-min
                          (count states)
                          (count states2)
                          (count states3)])
                    (cond (== t n-max)
                          (do
                            (prn [:result (:id bp) (->> states3
                                                        (map (fn [s] (-> s :inventory (:geode 0))))
                                                        (reduce max 0))])
                            (->> states3
                                 (map (fn [s] (-> s :inventory (:geode 0))))
                                 (reduce max 0)
                                 (* (:id bp))))
                          :else
                          (recur (inc t)
                                 (->> states3
                                      (mapcat (possible-moves bp))
                                      set)))))))
         (reduce +))))

(defn part2
  [input]
  (let [n-max 32]
    (->> input
         (take 3)
         (map (fn [bp]
                (loop [t 0
                       states #{{:robots {:ore 1} :inventory {}}}]
                  (let [
                        guaranteed-min (->> states
                                            (map (fn [state]
                                                   (+ (get-in state [:inventory :geode] 0)
                                                      (* (- n-max t) (get-in state [:robots :geode] 0)))))
                                            (reduce max 0))
                        states2 (filter (fn [state]
                                          (>= ((potential (- n-max t)) state)
                                              guaranteed-min))
                                        states)
                        f (fn [m]
                            (->> ((juxt :ore :clay :obsidian :geode) m)
                                 (mapv (fn [n] (or n 0)))))
                        mins (->> states2
                                  (reduce (fn [acc {:keys [robots inventory]}]
                                            (let [robs (f robots)
                                                  invs (f inventory)
                                                  prev (get acc robs)]
                                              (if (or (nil? prev)
                                                      (every? (fn [[a b]] (>= a b))
                                                              (map vector invs prev)))
                                                (assoc acc robs invs)
                                                acc)))
                                          {}))
                        states3 (->> states2
                                     (remove (fn [s]
                                               (let [robs (f (:robots s))
                                                     prev (get mins robs)
                                                     invs (f (:inventory s))]
                                                 (and (not (= invs prev))
                                                      (every? (fn [[a b]] (<= a b))
                                                              (map vector invs prev)))))))
                        ]
                    (prn [(:id bp) t
                          (count states)
                          (reduce max 0 (map (potential (- n-max t)) states))
                          (reduce min (map (potential (- n-max t)) states))
                          guaranteed-min
                          (count states)
                          (count states2)
                          (count states3)])
                    (cond (== t n-max)
                          (do
                            (prn [:result (:id bp) (->> states3
                                                        (map (fn [s] (-> s :inventory (:geode 0))))
                                                        (reduce max 0))])
                            (->> states3
                                 (map (fn [s] (-> s :inventory (:geode 0))))
                                 (reduce max 0)))
                          :else
                          (recur (inc t)
                                 (->> states3
                                      (mapcat (possible-moves bp))
                                      set)))))))
         (reduce *))))

(lib/check
  #_#_[part1 sample] 33
  #_#_[part1 puzzle] 1127
  [part2 sample] (* 56 62)
  [part2 puzzle] 0
  )

