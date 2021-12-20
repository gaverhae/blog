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
  (loop [ps [[init] 0]]
    (let [paths (get ps 0)
          num-paths (get ps 1)]
      (if (empty? paths)
        num-paths
        (recur (loop [paths paths
                      [ps np] [[] num-paths]]
                 (if (empty? paths)
                   [ps np]
                   (recur (rest paths)
                          (let [[pos state] (first paths)]
                            (loop [current ^longs (aget input (abs pos))
                                   current-end ^int (alength current)
                                   idx (int 0)
                                   ps ps
                                   np np]
                              (if (== current-end idx) [ps np]
                                (cond (== 1 (aget current idx))
                                      (recur current
                                             current-end
                                             (unchecked-inc-int idx)
                                             ps
                                             (inc np))
                                      (forbidden state (aget current idx))
                                      (recur current
                                             current-end
                                             (unchecked-inc-int idx)
                                             ps
                                             np)
                                      :else
                                      (recur current
                                             current-end
                                             (unchecked-inc-int idx)
                                             (conj ps [(aget current idx)
                                                       (update-state state (aget current idx))])
                                             np)))))))))))))

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
  [^"[[J" input]
  (let [size (alength input)
        init ^booleans (make-array Boolean/TYPE (inc size))]
    (aset init 0 true)
    (traverse input
              [0 init]
              (fn [^booleans visited cave]
                (or (zero? cave)
                    (and (aget visited size)
                         (aget visited (abs cave)))))
              (fn [^booleans visited ^long cave]
                (if (neg? cave)
                  (doto (aclone visited)
                    (aset size (or (aget visited size)
                                   (aget visited (abs cave))))
                    (aset (abs cave) true))
                  visited)))))
