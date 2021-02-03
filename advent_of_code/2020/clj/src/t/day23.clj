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
    (loop [cur (unchecked-dec (aget input 0))
           n (long 0)]
      (if (== n num-turns)
        (->> (iterate #(aget cups %) 0)
             (take num-cups)
             (map inc))
        (let [p1 (aget cups cur)
              p2 (aget cups p1)
              p3 (aget cups p2)
              nxt (aget cups p3)
              lbl (loop [lbl (unchecked-dec cur)]
                    (if (or (== lbl p1)
                            (== lbl p2)
                            (== lbl p3)
                            (== lbl (long -1)))
                      (if (== lbl (long -1))
                        (recur max-cup)
                        (recur (unchecked-dec lbl)))
                      lbl))
              post-p3 (aget cups lbl)]
          (aset cups cur nxt)
          (aset cups lbl p1)
          (aset cups p3 post-p3)
          (recur nxt
                 (unchecked-inc n)))))))

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
