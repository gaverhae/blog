(ns t.day16
  (:require [clojure.core.match :refer [match]]
            [instaparse.core :as insta]))

(def grammar
  "
  S = packet leftover
  packet = literal-packet | operator-packet
  literal-packet = version <'100'> literal
  <literal> = (repeated-lit)* ending-lit
  <version> = #'.{3}'
  <repeated-lit> = <'1'> #'.{4}'
  <ending-lit> = <'0'> #'.{4}'
  operator-packet = version operator-type-id length payload
  <operator-type-id> = '000' | '001' | '010' | '011' | '101' | '110' | '111'
  <length> = length-bits | length-packets
  length-bits = <'0'> #'.{15}'
  length-packets = <'1'> #'.{11}'
  <payload> = #'.*'
  leftover = #'.*'
  ")

(def to-ast (insta/parser grammar))

(defn parse-ast
  [tree]
  (prn tree)
  (match tree
    [:leftover ""] ()
    [:leftover s] (parse-ast (to-ast s))
    [:S packet leftover] (cons (parse-ast packet) (parse-ast leftover))
    [:packet packet] (parse-ast packet)
    [:literal-packet version & lit]
    {:version (Long/parseLong version 2)
     :type [:literal]
     :value (Long/parseLong (apply str lit) 2)}
    [:operator-packet version type [length-type length] payload]
    {:version (Long/parseLong version 2)
     :type [:operator (Long/parseLong type 2)]
     :payload (let [length (Long/parseLong length 2)]
                (case length-type
                  :length-bits (parse-ast (to-ast (subs payload 0 length)))
                  :length-packets (take length (parse-ast (to-ast payload)))))}
    ))

(defn parse
  [lines]
  (->> lines
       first
       (mapcat {\0 "0000" \1 "0001" \2 "0010" \3 "0011" \4 "0100" \5 "0101"
                \6 "0110" \7 "0111" \8 "1000" \9 "1001" \A "1010" \B "1011"
                \C "1100" \D "1101" \E "1110" \F "1111"})
       (apply str)
       to-ast
       parse-ast))

(defn part1
  [input]

  )

(defn part2
  [input])
