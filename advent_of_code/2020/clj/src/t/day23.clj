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
        cups (concat input (map inc (range (count input) num-cups)))]
    (loop [n 0
           prev cups]
      (if (== n num-turns)
        prev
        (let [cur (first prev)
              pck (take 3 (rest prev))
              lbl (loop [tgt (dec cur)]
                    (cond (zero? tgt) (recur num-cups)
                          ((set pck) tgt) (recur (dec tgt))
                          :else tgt))
              pos (loop [idx 4]
                    (if (== lbl (nth prev idx))
                      (- idx 3)
                      (recur (inc idx))))]
          (recur (inc n)
                 (concat (take pos (drop 4 prev))
                         pck
                         (drop (+ 4 pos) prev)
                         [cur])))))))

(defn part1
  [input]
  (let [cups (do-moves input 9 100)]
    (->> (concat (rest (drop-while #(not= 1 %) cups))
                 (take-while #(not= 1 %) cups))
         (map str)
         (string/join)
         (Long/parseLong))))

(defn part2
  [input]
  (->> (do-moves input (* 20 #_#_ 1000 1000) (* #_10 #_1000 100))
       (drop-while #(not= 1 %))
       (rest)
       (take 2)
       (apply *)))
