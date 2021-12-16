(ns t.day16
  (:require [clojure.core.match :refer [match]]))

(defn parse-literal
  [bits]
  (Long/parseLong (->> bits
                       (partition 5)
                       (mapcat rest)
                       (apply str))
                  2))

(defn parse-operator
  [[length-id & payload]]
  (let [length-type ({\0 :bits, \1 :packets} length-id)
        length-length ({:bits 15, :packets 11} length-type)
        length (Long/parseLong (apply str (take length-length payload)) 2)
        payload (drop length-length payload)]
    {:length [length-type length]
     :packets nil}))


(defn parse-packet
  [[v1 v2 v3 t1 t2 t3 & payload]]
  (let [version (Long/parseLong (str v1 v2 v3) 2)
        type-id (Long/parseLong (str t1 t2 t3) 2)]
    {:version version
     :type-id type-id
     :type ({4 :literal} type-id)
     :payload (match [version type-id]
                [_ 4] (parse-literal payload)
                [_ _] (parse-operator payload))}))

(defn parse
  [lines]
  (->> lines
       first
       (mapcat {\0 "0000" \1 "0001" \2 "0010" \3 "0011" \4 "0100" \5 "0101"
                \6 "0110" \7 "0111" \8 "1000" \9 "1001" \A "1010" \B "1011"
                \C "1100" \D "1101" \E "1110" \F "1111"})
       (apply str)
       parse-packet))

(defn part1
  [input]

  )

(defn part2
  [input])
