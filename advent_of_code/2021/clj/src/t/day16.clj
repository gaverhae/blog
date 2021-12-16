(ns t.day16
  (:require [clojure.core.match :refer [match]]
            [instaparse.core :as insta]))

(def grammar
  "
  S = packet <zeros>
  packet = literal-packet | operator-packet
  literal-packet = version <'100'> literal
  <literal> = (repeated-lit)* ending-lit
  <version> = #'...'
  <repeated-lit> = <'1'> #'....'
  <ending-lit> = <'0'> #'....'
  operator-packet = '11111'
  zeros = '0'*
  ")

(def to-ast (insta/parser grammar))

(defn parse-ast
  [tree]
  (prn tree)
  (match tree
    [:S packet] (parse-ast packet)
    [:packet packet] (parse-ast packet)
    [:literal-packet version & lit] {:version (Long/parseLong version 2)
                                     :type :literal
                                     :value (Long/parseLong (apply str lit) 2)}))

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
