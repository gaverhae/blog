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
  [^"[[J" input init-state forbidden update-state]
  (loop [q ()
         state init-state
         current ^longs (aget input 0)
         end ^int (alength current)
         idx (int 0)
         num-paths 0]
    (cond (and (== idx end)
               (empty? q))
          num-paths
          (== idx end)
          (let [next-item (first q)
                q (rest q)
                state (get next-item 1)
                current ^longs (aget input (abs (get next-item 0)))
                end ^int (alength current)
                idx (int 0)]
            (recur q state current end idx num-paths))
          :else
          (let [c (aget current idx)]
            (cond (== 1 c)
                  (recur q state current end (unchecked-inc-int idx) (inc num-paths))
                  (forbidden state c)
                  (recur q state current end (unchecked-inc-int idx) num-paths)
                  :else
                  (recur (conj q [c (update-state state c)])
                         state current end (unchecked-inc-int idx) num-paths))))))

(defn part1
  [input]
  (traverse input
            #{0}
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
              init
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
