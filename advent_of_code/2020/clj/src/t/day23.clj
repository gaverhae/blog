(ns t.day23
  (:require [clojure.string :as string]))

(defn parse
  [lines]
  (->> lines
       first
       (map #(Long/parseLong (str %)))))

(defn do-moves
  [^longs input ^long num-cups-l ^long num-turns-l]
  (let [num-cups (int num-cups-l)
        num-turns (int num-turns-l)
        input-size ^int (alength input)
        dec-input-size ^int (unchecked-dec-int input-size)
        max-cup ^int (unchecked-dec-int num-cups)
        cups ^longs (long-array num-cups)
        next-cup (fn ^long [^long n] (aget cups n))
        bef ^long (System/currentTimeMillis)]
    (dotimes [i num-cups]
      (cond
        (< i dec-input-size)
        (aset cups (unchecked-dec-int (aget input i))
                   (unchecked-dec-int (aget input (unchecked-inc-int i))))

        (== i dec-input-size)
        (if (> num-cups input-size)
          (aset cups (unchecked-dec-int (aget input i))
                     input-size)
          (aset cups (unchecked-dec-int (aget input i))
                     (unchecked-dec-int (aget input 0))))

        (< i max-cup)
        (aset cups i (unchecked-inc-int i))

        :else
        (aset cups i (unchecked-dec-int (aget input 0)))))
    #_(println "after: " (- (System/currentTimeMillis) bef))
    (loop [cur (dec (first input))
           n 0]
      (if (== n num-turns)
        (do #_(println "final: " (- (System/currentTimeMillis) bef))
            (->> (iterate next-cup 0)
                 (take num-cups)
                 (map inc)))
        (let [p1 (next-cup cur)
              p2 (next-cup p1)
              p3 (next-cup p2)
              nxt (next-cup p3)
              lbl (loop [tgt (mod (dec cur) num-cups)]
                    (if (#{p1 p2 p3} tgt)
                      (recur (mod (dec tgt) num-cups))
                      tgt))
              post-p3 (next-cup lbl)]
          (aset cups (int cur) (long nxt))
          (aset cups (int lbl) (long p1))
          (aset cups (int p3) (long post-p3))
          (recur nxt
                 (inc n)))))))

(defn part1
  [input]
  (->> (do-moves (long-array input) 9 100)
       (rest)
       (map str)
       (string/join)
       (Long/parseLong)))

(defn part2
  [input]
  (->> (do-moves (long-array input) (* 1000 1000) (* 10 1000 1000))
       (rest)
       (take 2)
       (apply *)))
