(ns t.day24)

(defn parse
  [lines]
  (->> lines
       (map (fn [l]
              (->> (re-seq #"e|se|sw|w|nw|ne" l)
                   (map {"se" [-1 0 1]
                         "sw" [0 -1 1]
                         "e" [-1 1 0]
                         "nw" [1 0 -1]
                         "ne" [0 1 -1]
                         "w" [1 -1 0]})
                   (reduce (fn [acc el]
                             [(+ (acc 0) (el 0))
                              (+ (acc 1) (el 1))
                              (+ (acc 2) (el 2))])
                           [0 0 0]))))
       (reduce (fn [acc el]
                 (update acc el (fnil not false)))
               {})
       (filter (fn [[pos black?]] black?))
       (map first)
       set))

(defn part1
  [input]
  (count input))

(defn part2
  [input]
  (let [encode (fn [[x y z]]
                 (+ x (bit-shift-left y 16) (bit-shift-left z 32)))
        inc-x (fn ^long [^long n] (unchecked-add n 1))
        dec-x (fn ^long [^long n] (unchecked-subtract n 1))
        inc-y (fn ^long [^long n] (unchecked-add n 65536))
        dec-y (fn ^long [^long n] (unchecked-subtract n 65536))
        inc-z (fn ^long [^long n] (unchecked-add n 4294967296))
        dec-z (fn ^long [^long n] (unchecked-subtract n 4294967296))
        neighbours (fn [n]
                     [(-> n dec-x inc-z)
                      (-> n dec-y inc-z)
                      (-> n dec-x inc-y)
                      (-> n inc-x dec-z)
                      (-> n inc-y dec-z)
                      (-> n inc-x dec-y)])]
    (->> (range 100)
         (reduce (fn [^longs prev _]
                   (java.util.Arrays/sort prev)
                   (let [counts (java.util.HashMap.)
                         black? (fn [^long n] (>= (java.util.Arrays/binarySearch prev n) 0))]
                     (doseq [p prev
                             n (neighbours p)]
                       (.put counts n (inc (.getOrDefault counts n 0))))
                     (->> counts
                          (keep (fn [[pos num-black-neighbours]]
                                  ;; if zero black neighbours, does not appear
                                  (when (or (and (or (== 1 num-black-neighbours)
                                                     (== 2 num-black-neighbours))
                                                 (black? pos))
                                            (and (== 2 num-black-neighbours)
                                                 (not (black? pos))))
                                    pos)))
                          (into-array Long/TYPE))))
                 (->> (map encode input)
                      (into-array Long/TYPE)))
         count)))

(comment
  (def in (-> (slurp "data/day24") clojure.string/split-lines parse))
  (defn go [] (part2 in))

  (go)

  )
