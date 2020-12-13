(ns t.day13
  (:require [clojure.string :as string]))

(defn parse
  [lines]
  [(Long/parseLong (first lines))
   (->> (string/split (second lines) #",")
        (map (fn [n] (case n "x" [:x] [:num (Long/parseLong n)]))))])

(defn part1
  [[arrival buses]]
  (->> buses
       (remove #{[:x]})
       (map second)
       (map (fn [n] [n (mod (- n (rem arrival n)) n)]))
       (sort-by second)
       first
       (apply *)))

(defn chinese-remainder-gauss
  [coefs]
  (let [N (->> coefs (map second) (reduce *))
        ex-gcd (fn [a b]
                 (cond
                   (zero? a) [(Math/abs (long b)) 0 1]
                   (zero? b) [(Math/abs (long a)) 1 0]
                   :else
                   (loop [s 0
                          s0 1
                          t 1
                          t0 0
                          r (Math/abs (long b))
                          r0 (Math/abs (long a))]
                     (if (zero? r)
                       [r0 s0 t0]
                       (let [q (quot r0 r)]
                         (recur (- s0 (* q s)) s
                                (- t0 (* q t)) t
                                (- r0 (* q r)) r))))))
        invmod (fn [a b]
                 (let [[g x y] (ex-gcd a b)]
                   x))
        sol (reduce (fn [acc [ai ni]]
                      (let [bi (/ N ni)]
                        (+ acc (* ai bi (invmod bi ni)))))
                    0
                    coefs)]
    (mod sol N)))

(defn part2
  [[_ buses]]
  ; https://rosettacode.org/wiki/Chinese_remainder_theorem
  (let [coefs (->> buses
                   (keep-indexed (fn [idx bus] (when (not (#{[:x]} bus)) [(- idx) (second bus)])))
                   vec)]
    (chinese-remainder-gauss coefs)))
