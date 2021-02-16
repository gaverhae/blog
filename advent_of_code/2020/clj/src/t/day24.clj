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

(defn encode ^long [[x y z]]
  (+ x (bit-shift-left y 16) (bit-shift-left z 32)))
(defn inc-x ^long [^long n] (unchecked-add n 1))
(defn dec-x ^long [^long n] (unchecked-subtract n 1))
(defn inc-y ^long [^long n] (unchecked-add n 65536))
(defn dec-y ^long [^long n] (unchecked-subtract n 65536))
(defn inc-z ^long [^long n] (unchecked-add n 4294967296))
(defn dec-z ^long [^long n] (unchecked-subtract n 4294967296))

(defn neighbours
  ^longs [^long n]
  (let [ret (long-array 6)]
    (aset ret 0 (-> n dec-x inc-z))
    (aset ret 1 (-> n dec-y inc-z))
    (aset ret 2 (-> n dec-x inc-y))
    (aset ret 3 (-> n inc-x dec-z))
    (aset ret 4 (-> n inc-y dec-z))
    (aset ret 5 (-> n inc-x dec-y))
    ret))

(defn counts
  ^java.util.Map [^longs prev]
  (let [ret (java.util.HashMap.)]
    (areduce prev p-idx _ nil
             (let [neighs ^longs (neighbours (aget prev p-idx))]
               (areduce neighs n-idx _ nil
                        (let [n (aget neighs n-idx)]
                          (.put ret n (unchecked-add 1 (long (.getOrDefault ret n 0))))))))
    ret))

(defn step
  [^longs prev]
  (java.util.Arrays/sort prev)
  (let [black? (fn [^long n] (>= (java.util.Arrays/binarySearch prev n) 0))
        kept (java.util.stream.LongStream/builder)]
    (doseq [^java.util.Map$Entry e (.entrySet (counts prev))]
      (let [pos (.getKey e)
            num-black-neighbours (.getValue e)]
        ;; if zero black neighbours, does not appear
        (when (or (and (or (== 1 num-black-neighbours)
                           (== 2 num-black-neighbours))
                       (black? pos))
                  (and (== 2 num-black-neighbours)
                       (not (black? pos))))
          (.accept kept pos))))
    (.toArray (.build kept))))

(defn part2
  [input]
  (->> (map encode input)
       (into-array Long/TYPE)
       (iterate step)
       (drop 100)
       first
       count))

(comment
  (def in (-> (slurp "data/day24") clojure.string/split-lines parse))
  (defn go [] (part2 in))

  (go)

  )
