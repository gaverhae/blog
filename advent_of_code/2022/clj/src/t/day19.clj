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
  [state]
  (->> (cond-> [state]
         (>= (state 10) (state 0)) (conj (-> state
                                             (update 10 - (state 0))
                                             (update 6 inc)))
         (>= (state 10) (state 1)) (conj (-> state
                                             (update 10 - (state 1))
                                             (update 7 inc)))
         (and (>= (state 10) (state 2))
              (>= (state 11) (state 3))) (conj (-> state
                                                   (update 10 - (state 2))
                                                   (update 11 - (state 3))
                                                   (update 8 inc)))
         (and (>= (state 10) (state 4))
              (>= (state 12) (state 5))) (conj (-> state
                                                   (update 10 - (state 4))
                                                   (update 12 - (state 5))
                                                   (update 9 inc))))
       (map (fn [s] (-> s
                        (update 10 + (state 6))
                        (update 11 + (state 7))
                        (update 12 + (state 8))
                        (update 13 + (state 9))
                        (update 14 dec))))))

(let [t (memoize (fn [n] (reduce + (range 1 (inc n)))))]
  (defn upper-bound
    [state]
    (let [steps-left (state 14)
          triangle (t steps-left)
          obs-clay-cost (state 3)
          obs-ore-cost (state 2)
          obsidian-cost (state 5)
          ore-cost (state 4)
          obsidian-upper-bound (+ (state 12)
                                  (* steps-left (state 8))
                                  triangle)
          ore-upper-bound (+ (state 10)
                             (* steps-left (state 6))
                             triangle)
          clay-upper-bound (+ (state 11)
                              (* steps-left (state 7))
                              triangle)
          max-obsidian-under-co (+ (state 12)
                                   (* steps-left
                                      (min (quot ore-upper-bound obs-ore-cost)
                                           (quot clay-upper-bound obs-clay-cost))))
          geode-so-far (state 13)
          geode-robots (state 9)]
      (+ geode-so-far
         (min (-> (reduce (fn [s _]
                            (-> (if (and (>= (s 10) ore-cost)
                                         (>= (s 12) obsidian-cost))
                                  (-> s
                                      (update 10 - ore-cost)
                                      (update 12 - obsidian-cost)
                                      (update 9 inc))
                                  (-> s
                                      (update 6 inc)
                                      (update 7 inc)
                                      (update 8 inc)))
                                (update 10 + (s 6))
                                (update 11 + (s 7))
                                (update 12 + (s 8))
                                (update 13 + (s 9))))
                          state
                          (range steps-left))
                  (get 13)
                  (- geode-so-far))
              (* steps-left (+ geode-robots
                               (quot max-obsidian-under-co obsidian-cost)))
              (* steps-left (+ geode-robots
                               (quot obsidian-upper-bound obsidian-cost)))
              (* steps-left (+ geode-robots
                               (quot ore-upper-bound ore-cost)))
              (+ (* steps-left geode-robots)
                 triangle))))))

(defn lower-bound
  [state]
  (+ (state 13) (* (state 14) (state 9))))

(defn solve
  [n-max input]
  (->> input
       (map (fn [bp]
              (loop [states #{[(-> bp :blueprints :ore :ore)
                               (-> bp :blueprints :clay :ore)
                               (-> bp :blueprints :obsidian :ore)
                               (-> bp :blueprints :obsidian :clay)
                               (-> bp :blueprints :geode :ore)
                               (-> bp :blueprints :geode :obsidian)
                               1 0 0 0
                               0 0 0 0
                               n-max]}]
                (let [guaranteed-min (->> states
                                          (map lower-bound)
                                          (reduce max 0))
                      states2 (filter (fn [state]
                                        (>= (upper-bound state)
                                            guaranteed-min))
                                      states)
                      mins (->> states2
                                (reduce (fn [acc state]
                                          (let [robs (subvec state 6 10)
                                                invs (subvec state 10 14)
                                                prev (get acc robs)]
                                            (if (or (nil? prev)
                                                    (every? (fn [[a b]] (>= a b))
                                                            (map vector invs prev)))
                                              (assoc acc robs invs)
                                              acc)))
                                        {}))
                      states3 (->> states2
                                   (remove (fn [s]
                                             (let [robs (subvec s 6 10)
                                                   prev (get mins robs)
                                                   invs (subvec s 10 14)]
                                               (and (not (= invs prev))
                                                    (every? (fn [[a b]] (<= a b))
                                                            (map vector invs prev)))))))]
                  (prn [(:id bp) (get (first states) 14)
                        (count states3)
                        (reduce max (map upper-bound states3))
                        (reduce min (map upper-bound states3))
                        guaranteed-min])
                  (cond (zero? (get (first states) 14))
                        (do
                          (prn [:result (:id bp) (->> states3
                                                      (map (fn [s] (s 13)))
                                                      (reduce max 0))])
                          [(:id bp)
                           (->> states3
                                (map (fn [s] (s 13)))
                                (reduce max 0))])
                        :else
                        (recur (->> states3
                                    (mapcat possible-moves)
                                    set)))))))))

(defn part1
  [input]
  (->> input
       (solve 24)
       (map (fn [[id res]] (* id res)))
       (reduce +)))

(defn part2
  [input]
  (->> input
       (take 3)
       (solve 32)
       (map (fn [[_ res]] res))
       (reduce *)))

(lib/check
  [part1 sample] 33
  [part1 puzzle] 1127
  [part2 sample] (* 56 62)
  [part2 puzzle] 21546)
