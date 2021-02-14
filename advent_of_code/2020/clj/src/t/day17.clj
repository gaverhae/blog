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
                                           offsets)))))]
    (->> (reduce (fn [prev _]
                   (->> prev
                        (map neighbours)
                        (reduce (fn [acc ^longs neighs]
                                  (areduce neighs idx ret acc
                                           (update ret (aget neighs idx) (fnil inc 0))))
                                {})
                        (keep (fn [[x n]]
                                (when (or (and (prev x) (#{2 3} n))
                                          (and (not (prev x)) (= 3 n)))
                                  x)))
                        set))
                 (set (map encode input))
                 (range 6))
         count)))

(defn part2
  [input]
  (part1 (set (map (fn [[x y z]] [x y z 0]) input))))

(comment

  (def in (parse (clojure.string/split-lines (slurp "data/day17"))))

  (defn go [] (part2 in))

  (def encode
  (fn [v] (->> v
               (map (fn [i] (+ 50 i)))
               (reduce (fn [acc el]
                         (+ el (* 100 acc)))
                       0))))

  (encode [1 -1 0])
514950

(require '[criterium.core :as crit :refer [bench]])

(defn nei [v]
  (reduce (fn [n idx]
            (mapcat (fn [t] [t (update t idx inc) (update t idx dec)]) n))
          #{v}
          (range (count v))))

(reduce (fn [n idx]
          (mapcat (fn [t] [t (update t idx (fn [s] `(inc ~s))) (update t idx (fn [s] `(dec ~s)))]) n))
        #{['x 'y 'z]}
        (range 3))

(reduce (fn [n idx]
          (mapcat (fn [t] [t (+ t idx) (- t idx)])
                  n))
        #{0}
        (take 3 (iterate #(* 100 %) 1)))

(defn nei-vec [[x y z]]
[[x y z] [x y (clojure.core/inc z)] [x y (clojure.core/dec z)] [x (clojure.core/inc y) z] [x (clojure.core/inc y) (clojure.core/inc z)] [x (clojure.core/inc y) (clojure.core/dec z)] [x (clojure.core/dec y) z] [x (clojure.core/dec y) (clojure.core/inc z)] [x (clojure.core/dec y) (clojure.core/dec z)] [(clojure.core/inc x) y z] [(clojure.core/inc x) y (clojure.core/inc z)] [(clojure.core/inc x) y (clojure.core/dec z)] [(clojure.core/inc x) (clojure.core/inc y) z] [(clojure.core/inc x) (clojure.core/inc y) (clojure.core/inc z)] [(clojure.core/inc x) (clojure.core/inc y) (clojure.core/dec z)] [(clojure.core/inc x) (clojure.core/dec y) z] [(clojure.core/inc x) (clojure.core/dec y) (clojure.core/inc z)] [(clojure.core/inc x) (clojure.core/dec y) (clojure.core/dec z)] [(clojure.core/dec x) y z] [(clojure.core/dec x) y (clojure.core/inc z)] [(clojure.core/dec x) y (clojure.core/dec z)] [(clojure.core/dec x) (clojure.core/inc y) z] [(clojure.core/dec x) (clojure.core/inc y) (clojure.core/inc z)] [(clojure.core/dec x) (clojure.core/inc y) (clojure.core/dec z)] [(clojure.core/dec x) (clojure.core/dec y) z] [(clojure.core/dec x) (clojure.core/dec y) (clojure.core/inc z)] [(clojure.core/dec x) (clojure.core/dec y) (clojure.core/dec z)]])

(defmacro b [e] `(first (:mean (crit/benchmark ~e {}))))

(defn nei-int [^long i]
[i
 (unchecked-add i 10000)
 (unchecked-add i -10000)
 (unchecked-add i 100)
 (unchecked-add i 10100)
 (unchecked-add i -9900)
 (unchecked-add i -100)
 (unchecked-add i 9900)
 (unchecked-add i -10100)
 (unchecked-add i 1)
 (unchecked-add i 10001)
 (unchecked-add i -9999)
 (unchecked-add i 101)
 (unchecked-add i 10101)
 (unchecked-add i -9899)
 (unchecked-add i -99)
 (unchecked-add i 9901)
 (unchecked-add i -10099)
 (unchecked-add i -1)
 (unchecked-add i 9999)
 (unchecked-add i -10001)
 (unchecked-add i 99)
 (unchecked-add i 10099)
 (unchecked-add i -9901)
 (unchecked-add i -101)
 (unchecked-add i 9899)
 (unchecked-add i -10101)]
  )

(defn nei-arr ^longs [^long i]
  (doto ^longs (long-array 26)
    (aset 0 (unchecked-add i 10000))
    (aset 1 (unchecked-add i -10000))
    (aset 2 (unchecked-add i 100)   )
    (aset 3 (unchecked-add i 10100) )
    (aset 4 (unchecked-add i -9900) )
    (aset 5 (unchecked-add i -100)  )
    (aset 6 (unchecked-add i 9900)  )
    (aset 7 (unchecked-add i -10100))
    (aset 8 (unchecked-add i 1)     )
    (aset 9 (unchecked-add i 10001) )
    (aset 10 (unchecked-add i -9999) )
    (aset 11 (unchecked-add i 101)   )
    (aset 12 (unchecked-add i 10101) )
    (aset 13 (unchecked-add i -9899) )
    (aset 14 (unchecked-add i -99)   )
    (aset 15 (unchecked-add i 9901)  )
    (aset 16 (unchecked-add i -10099))
    (aset 17 (unchecked-add i -1)    )
    (aset 18 (unchecked-add i 9999)  )
    (aset 19 (unchecked-add i -10001))
    (aset 20 (unchecked-add i 99)    )
    (aset 21 (unchecked-add i 10099) )
    (aset 22 (unchecked-add i -9901) )
    (aset 23 (unchecked-add i -101)  )
    (aset 24 (unchecked-add i 9899)  )
    (aset 25 (unchecked-add i -10101))
    ))

(b (nei [1 2 3]))
4.395512656638458E-6

(b (nei-vec [1 2 3]))
4.2312273984130804E-7

(b (nei-int 514950))
1.1762816901491361E-7

(b (nei-arr 514950))
2.4585955424968558E-8

  )
