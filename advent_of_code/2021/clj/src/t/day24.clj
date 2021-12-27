(ns t.day24
  (:require [clojure.core.match :refer [match]]))

(defn parse
  [lines]
  (->> lines
       (map (fn [l] (let [[_ op arg1 _ arg2] (re-matches #"(...) (.)( (-?.+))?" l)]
                      [(keyword op) (keyword arg1) arg2])))
       (map (fn [[op arg1 arg2]]
              (if (= op :inp)
                [:inp arg1]
                [op arg1 (if (Character/isLetter ^Character (first arg2))
                           [:reg (keyword arg2)]
                           [:lit (Long/parseLong arg2)])])))))

(defn to-expr
  [instrs]
  (reduce (fn [acc instr]
            (match instr
              [:inp r] (-> acc
                           (assoc r [:inp (:input-count acc)])
                           (update :input-count inc))
              [:add _ [:lit 0]] acc
              [:add r1 [:lit n]] (update acc r1 (fn [prev] [:add prev [:lit n]]))
              [:add r1 [:reg r2]] (update acc r1 (fn [prev] [:add prev (acc r2)]))
              [:mul r1 [:lit 0]] (assoc acc r1 [:lit 0])
              [:mul r1 [:lit 1]] acc
              [:mul r1 [:lit n]] (update acc r1 (fn [prev] [:mul prev [:lit n]]))
              [:mul r1 [:reg r2]] (update acc r1 (fn [prev] [:mul prev (acc r2)]))
              [:div r1 [:lit 1]] acc
              [:div r1 [:lit n]] (update acc r1 (fn [prev] [:div prev [:lit n]]))
              [:div r1 [:reg r2]] (update acc r1 (fn [prev] [:div prev (acc r2)]))
              [:mod r1 [:lit n]] (update acc r1 (fn [prev] [:mod prev [:lit n]]))
              [:mod r1 [:reg r2]] (update acc r1 (fn [prev] [:mod prev (acc r2)]))
              [:eql r1 [:lit n]] (update acc r1 (fn [prev] [:eql prev [:lit n]]))
              [:eql r1 [:reg r2]] (update acc r1 (fn [prev] [:eql prev (acc r2)]))))
          {:w [:lit 0], :x [:lit 0], :y [:lit 0], :z [:lit 0], :input-count 0}
          instrs))

(defn part1
  [input]
  (to-expr input))

(defn part2
  [input]
  (count (part1 input)))
