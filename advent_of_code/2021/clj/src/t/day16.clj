(ns t.day16
  (:require [clojure.core.match :refer [match]]
            [instaparse.core :as insta]))

(def grammar
  "
  S = packet zeros
  packet = literal-packet | operator-packet
  literal-packet = version <'100'> literal
  <literal> = (repeated-lit)* ending-lit
  <version> = #'.{3}'
  <repeated-lit> = <'1'> #'.{4}'
  <ending-lit> = <'0'> #'.{4}'
  operator-packet = version operator-type-id length packets
  <packets> = packet*
  <operator-type-id> = '000' | '001' | '010' | '011' | '101' | '110' | '111'
  <length> = length-bits | length-packets
  length-bits = <'0'> #'.{15}'
  length-packets = <'1'> #'.{11}'
  zeros = <'0'*>
  ")

(def to-ast (insta/parser grammar))

(defn parse-ast
  [tree]
  (match tree
    [:S packet [:zeros]] [(parse-ast packet)]
    [:S packet [:more-data data]] (cons (parse-ast packet) (parse-ast (to-ast data)))
    [:packet packet] (parse-ast packet)
    [:literal-packet version & lit]
    {:version (Long/parseLong version 2)
     :type [:literal]
     :value (Long/parseLong (apply str lit) 2)}
    [:operator-packet version type [length-type length] & packets]
    {:version (Long/parseLong version 2)
     :type [:operator (Long/parseLong type 2)]
     :length [({:length-bits :bits, :length-packets :packets} length-type)
              (Long/parseLong length 2)]
     :payload (map parse-ast packets)}))

(defn parse-bits
  [bits]
  (letfn [(parse-packet [bits]
            (let [version (Long/parseLong (subs bits 0 3) 2)
                  type (Long/parseLong (subs bits 3 6) 2)
                  bits (subs bits 6)]
              (if (= type 4)
                (let [[value bits] (parse-value bits)]
                  [[:literal version value]
                   bits])
                (let [[args bits] (parse-args bits)]
                  [[:operator version type args]
                   bits]))))
          (parse-value [bits]
            (loop [bits bits
                   so-far ""]
              (let [continue? (= "1" (subs bits 0 1))
                    so-far (str so-far (subs bits 1 5))
                    bits (subs bits 5)]
                (if continue?
                  (recur bits so-far)
                  [(Long/parseLong so-far 2) bits]))))
          (parse-args [bits]
            (let [length-type ({"0" :bits, "1" :packets} (subs bits 0 1))
                  bits (subs bits 1)]
              (case length-type
                :bits (let [length (Long/parseLong (subs bits 0 15) 2)
                            bits (subs bits 15)
                            packets (subs bits 0 length)
                            bits (subs bits length)]
                        [(loop [so-far []
                                bits packets]
                           (if (empty? bits)
                             so-far
                             (let [[p bits] (parse-packet bits)]
                               (recur (conj so-far p) bits))))
                         bits])
                :packets (let [length (Long/parseLong (subs bits 0 11) 2)
                               bits (subs bits 11)]
                           (loop [so-far []
                                  bits bits]
                             (if (= (count so-far) length)
                               [so-far bits]
                               (let [[p bits] (parse-packet bits)]
                                 (recur (conj so-far p) bits))))))))]
    (let [[top-level-packet bits] (parse-packet bits)]
      (when-not (every? #{\0} bits)
        (throw (RuntimeException. ^String bits)))
      top-level-packet)))

(defn parse
  [lines]
  (->> lines
       first
       (mapcat {\0 "0000" \1 "0001" \2 "0010" \3 "0011" \4 "0100" \5 "0101"
                \6 "0110" \7 "0111" \8 "1000" \9 "1001" \A "1010" \B "1011"
                \C "1100" \D "1101" \E "1110" \F "1111"})
       (apply str)
       parse-bits))

(defn part1
  [input])

(defn part2
  [input])
