(ns t.day15
  (:require [clojure.string :as string]))

(defn parse
  [[line]]
  (mapv #(Long/parseLong %) (string/split line #",")))

(defn brute-force
  [stop input]
  (let [prevs (int-array stop)]
    (dotimes [n (dec (count input))]
      (aset ^"[I" prevs (unchecked-int (input n)) (unchecked-inc-int (unchecked-int n))))
    (loop [n (unchecked-int (count input))
           cur (unchecked-int (last input))]
      (let [last-said (unchecked-int (aget ^"[I" prevs (unchecked-int cur)))
            _ (aset ^"[I" prevs (unchecked-int cur) (unchecked-int n))]
        (cond (zero? (unchecked-subtract-int (unchecked-int n) (unchecked-int stop))) cur
              (zero? (unchecked-int last-said)) (recur (unchecked-inc-int n) (unchecked-int 0))
              :else (recur (unchecked-inc-int n)
                           (unchecked-subtract-int (unchecked-int n) (unchecked-int last-said))))))))

(defn part1
  [input]
  (brute-force 2020 input))

(def part2
  (memoize (fn
  [input]
  (brute-force 30000000 input))
           ))
