(ns t.day17
  (:require [clojure.set :as set]))

(defn parse
  [lines]
  (->> lines
       (map-indexed (fn [y line] (->> line (keep-indexed (fn [x c] (when (= c \#) [x y 0]))))))
       (apply concat)
       set))

(defn part1
  [input]
  (let [size ^long (->> input first count)
        encode (fn [v] (->> v
                            (map (fn [i] (+ 50 i)))
                            (reduce (fn [acc el]
                                      (+ el (* 100 acc)))
                                    0)))
        offsets (->> (iterate #(* 100 %) 1)
                     (take size)
                     (reduce (fn [n idx]
                               (mapcat (fn [t] [t (+ t idx) (- t idx)])
                                       n))
                             [0])
                     rest)
        neighbours (eval
                     (let [tagged-arg (with-meta 'i {:tag long})
                           tagged-args (with-meta [tagged-arg] {:tag longs})
                           tagged-arr (with-meta `(long-array ~(count offsets)) {:tag longs})]
                       `(fn ~tagged-args
                          (doto ~tagged-arr
                            ~@(map-indexed (fn [idx o] `(aset ~idx (long (unchecked-add ~tagged-arg (long ~o)))))
                                           offsets)))))
        active? (fn [^longs prev ^long c]
                  (not (neg? (java.util.Arrays/binarySearch prev c))))
        step (fn ^longs [^longs prev]
               (java.util.Arrays/sort prev)
               (let [cur (java.util.HashMap.)
                     nex (java.util.ArrayList. (alength prev))
                     _ (areduce prev p-idx _ nil
                                (let [neighs ^longs (neighbours (aget prev p-idx))]
                                  (areduce neighs n-idx _ nil
                                           (.put cur (aget neighs n-idx) (inc (.getOrDefault cur (aget neighs n-idx) 0))))
                                  cur))
                     entries ^"[Ljava.util.Map$Entry;" (.toArray (.entrySet cur))
                     _ (areduce entries e-idx _ nil
                                (let [e ^java.util.Map$Entry (aget entries e-idx)
                                      x (long (.getKey e))
                                      n (long (.getValue e))]
                                  (when (or (and (active? prev x) (or (== 2 n) (== 3 n)))
                                            (and (not (active? prev x)) (== 3 n)))
                                    (.add nex x))))
                     boxed (.toArray nex)
                     ret (long-array (alength boxed))]
                 (areduce boxed b-idx _ nil
                          (aset ret b-idx (long (aget boxed b-idx))))
                 ret))]
    (loop [n 0
           s ^longs (long-array (map encode input))]
      (if (== 6 n)
        (alength s)
        (recur (inc n)
               (step s))))))

(defn part2
  [input]
  (part1 (set (map (fn [[x y z]] [x y z 0]) input))))

(comment

  (def in (parse (clojure.string/split-lines (slurp "data/day17"))))

  (defn go [] (part2 in))

(require '[criterium.core :as crit :refer [bench]])

(defmacro b [e] `(first (:mean (crit/benchmark ~e {}))))

(b (inc 1))
1.2665377215845391E-9

(b (go))

(let [n 10000
      start (System/currentTimeMillis)]
  (dotimes [_ n]
    (go))
  (/ (- (System/currentTimeMillis) start)
     n))
(* 1.0 517317/10000)
51.7317


  )


