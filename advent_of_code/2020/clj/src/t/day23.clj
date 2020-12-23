(ns t.day23
  (:require [clojure.string :as string]))

(defn parse
  [lines]
  (->> lines
       first
       (map #(Long/parseLong (str %)))))

(defn do-moves
  [input num-cups num-turns]
  (let [num-labels (count input)
        cups (concat input (map inc (range (count input) num-cups)))
        links (long-array num-cups)
        next-cup (fn ^long [^long n] (aget links n))]
    (doseq [[^long c1 ^long c2] (->> cups
                                      (map dec)
                                      (partition 2 1 [(dec (first input))]))]
      (aset links c1 c2))
    (loop [cur (dec (first input))
           n 0]
      (if (== n num-turns)
        (->> (iterate next-cup 0)
             (take num-cups)
             (map inc))
        (let [p1 (next-cup cur)
              p2 (next-cup p1)
              p3 (next-cup p2)
              nxt (next-cup p3)
              lbl (loop [tgt (mod (dec cur) num-cups)]
                    (if (#{p1 p2 p3} tgt)
                      (recur (mod (dec tgt) num-cups))
                      tgt))
              post-p3 (next-cup lbl)]
          (aset links (int cur) (long nxt))
          (aset links (int lbl) (long p1))
          (aset links (int p3) (long post-p3))
          (recur nxt
                 (inc n)))))))

(defn part1
  [input]
  (->> (do-moves input 9 100)
       (rest)
       (map str)
       (string/join)
       (Long/parseLong)))

(defn part2
  [input]
  (->> (do-moves input (* 1000 1000) (* 10 1000 1000))
       (rest)
       (take 2)
       (apply *)))
