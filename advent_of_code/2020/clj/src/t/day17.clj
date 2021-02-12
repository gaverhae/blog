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
        incs [#(+ % 1) #(+ % 100) #(+ % 10000) #(+ % 1000000)]
        decs [#(- % 1) #(- % 100) #(- % 10000) #(- % 1000000)]
        neighbourhood (memoize
                        (fn [v]
                          (reduce (fn [n idx]
                                    (set (mapcat (fn [t] [t ((get incs idx) t) ((get decs idx) t)]) n)))
                                  #{v}
                                  (range size))))
        neighbours (fn [v]
                     (disj (neighbourhood v) v))
        to-check (fn [active]
                   (->> active
                        (mapcat neighbourhood)
                        set))]
    (->> (reduce (fn [prev _]
                   (set (for [v (to-check prev)
                              :let [active? (prev v)
                                    active-neighbours (count (set/intersection (neighbours v) prev))]
                              :when (or (and active? (#{2 3} active-neighbours))
                                        (and (not active?) (= 3 active-neighbours)))]
                          v)))
                 (set (map encode input))
                 (range 6))
         count)))

(defn part2
  [input]
  (part1 (set (map (fn [[x y z]] [x y z 0]) input))))
