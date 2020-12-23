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
        links (->> cups
                   (map dec)
                   (partition 2 1 [(dec (first input))])
                   (reduce (fn [acc [c1 c2]]
                             (assoc acc c1 c2))
                           {}))]
    (loop [cur (dec (first input))
           next-cup links
           n 0]
      (if (= n num-turns)
        (->> (iterate next-cup 0)
             (take num-cups)
             (map inc))
        (let [[_ p1 p2 p3 nxt] (take 5 (iterate next-cup cur))
              lbl (loop [tgt (mod (dec cur) num-cups)]
                    (if (#{p1 p2 p3} tgt)
                      (recur (mod (dec tgt) num-cups))
                      tgt))]
          (recur nxt
                 (-> next-cup
                     (assoc cur nxt)
                     (assoc lbl p1)
                     (assoc p3 (next-cup lbl)))
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
