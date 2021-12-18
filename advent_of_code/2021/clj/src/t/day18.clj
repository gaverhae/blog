(ns t.day18
  (:require [clojure.core.match :refer [match]]))

(defn parse
  [lines]
  (->> lines
       (map read-string)))

(defn decode
  [n]
  (let [h (fn rec [depth n]
            (->> n
                 (mapcat (fn [n]
                           (if (vector? n)
                             (rec (inc depth) n)
                             [[n depth]])))))]
    (vec (h 0 n))))

(defn encode
  [ls]
  (let [step (fn step [[[fst d1] [snd d2] & tl :as ls]]
               (if (== d1 d2)
                 (vec (cons [[fst snd] (dec d1)] tl))
                 (cons [fst d1] (step (rest ls)))))]
    (loop [[[fst d] :as ls] ls]
      (if (== -1 d)
        fst
        (recur (step ls))))))

(defn exp-enc
  [ls]
  (cond (== (count ls) 1) ls
        (== 4 (second (first ls))) (let [[[fst _] [snd _] [trd d] & tl] ls]
                                     (list* [0 3] [(+ snd trd) d] tl))
        (== 4 (second (second ls))) (let [[[fst d1] [snd _] [trd _] [frh d2] & tl] ls]
                                      (if frh
                                        (list* [(+ fst snd) d1] [0 3] [(+ trd frh) d2] tl)
                                        (list* [(+ fst snd) d1] [0 3] tl)))
        :else (cons (first ls) (exp-enc (rest ls)))))

(defn explode
  [n]
  #_(prn [:explode_1 n])
  #_(prn [:explode_2 (-> n
      decode
      exp-enc
      encode)])
  (-> n
      decode
      exp-enc
      encode))

(defn split-enc
  [ls]
  #_(prn ls)
  (if (seq ls)
    (let [[[fst depth :as hd] & tl] ls]
      #_(prn [:fst fst :hd hd :tl tl])
      (if (>= fst 10)
        (list* [(quot fst 2) (inc depth)]
               [(quot (inc fst) 2) (inc depth)]
               tl)
        (cons hd (split-enc tl))))))

(defn split
  [n]
  #_(prn [:split_1 n])
  #_(prn [:split_2 (-> n
                   decode
                   split-enc
                   encode)])
  (-> n
      decode
      split-enc
      encode))

(defn reduce-num
  [n]
  (loop [prev n]
    (let [n (explode prev)]
      (if (= n prev)
        (let [n (split prev)]
          (if (= n prev)
            n
            (recur n)))
        (recur n)))))

(defn sum
  [n1 n2]
  (reduce-num (vector n1 n2)))

(defn magnitude
  [n]
  (if (vector? n)
    (+ (* 3 (magnitude (first n)))
       (* 2 (magnitude (second n))))
    n))

(defn part1
  [input]
  (->> input
       (reduce sum)
       magnitude))

(defn part2
  [input]
  (->> (for [n1 input
             n2 input
             :when (not= n1 n2)]
         (magnitude (sum n1 n2)))
       (reduce max)))
