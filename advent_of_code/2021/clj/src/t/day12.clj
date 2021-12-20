(ns t.day12
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn abs
  [^long l]
  (if (neg? l) (- l) l))

(defn parse
  [lines]
  (let [links (->> lines
                   (map #(string/split % #"-")))
        caves (->> links
                   (apply concat)
                   set
                   (remove #{"start" "end"})
                   sort)
        mapping (->> caves
                     (map-indexed (fn [idx ^String cave]
                                    [cave (* (if (= cave (.toLowerCase cave))
                                               -1 1)
                                             (+ 2 idx))]))
                     (into {"start" 0
                            "end" 1}))
        as-map (reduce (fn [system [from to]]
                         (let [from (mapping from)
                               to (mapping to)]
                           (-> system
                               (update from (fnil conj #{}) to)
                               (update to (fnil conj #{}) from))))
                       {}
                       links)
        num-caves (->> caves count inc inc)]
    (reduce (fn [^"[[J" arr [^long idx v]]
              (aset arr (abs idx) (into-array Long/TYPE (sort-by abs v)))
              arr)
            (make-array Long/TYPE num-caves 0)
            as-map)))

(defn traverse
  [^"[[J" input init forbidden update-state]
  (loop [[paths num-paths] [[init] 0]]
    (if (empty? paths)
      num-paths
      (recur (reduce (fn [[ps np] [pos state]]
                       (let [outlinks ^longs (aget input (abs pos))
                             end ^int (alength outlinks)]
                         (loop [idx (int 0)
                                [ps np] [ps np]]
                           (if (== end idx) [ps np]
                             (recur (unchecked-inc-int idx)
                                    (cond (== 1 (aget outlinks idx)) [ps (inc np)]
                                          (forbidden state (aget outlinks idx)) [ps np]
                                          :else [(conj ps [(aget outlinks idx)
                                                           (update-state state (aget outlinks idx))])
                                                 np]))))))
                     [[] num-paths]
                     paths)))))

(defn part1
  [input]
  (traverse input
            [0 #{0}]
            contains?
            (fn [visited cave]
              (if (neg? cave)
                (conj visited cave)
                visited))))

(defn part2
  [input]
  (traverse input
            [0 [#{0} false]]
            (fn [[visited twice?] cave]
              (or (= 0 cave)
                  (and (visited cave) twice?)))
            (fn [[visited twice?] cave]
              [(if (neg? cave)
                 (conj visited cave)
                 visited)
               (or twice?
                   (boolean (visited cave)))])))
