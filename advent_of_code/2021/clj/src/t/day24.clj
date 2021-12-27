(ns t.day24
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as walk]))

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
  (->> instrs
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
               {:w [:lit 0], :x [:lit 0], :y [:lit 0], :z [:lit 0], :input-count 0})
       :z))

(defn compute-range
  [op]
  (match op
    [:add e1 e2] (let [r1 (compute-range e1)
                       r2 (compute-range e2)]
                   [(+ (apply min r1)
                       (apply min r2))
                    (+ (apply max r1)
                       (apply max r2))])
    [:mul e1 e2] (let [r1 (compute-range e1)
                       r2 (compute-range e2)]
                   [(+ (apply min r1)
                       (apply min r2))
                    (+ (apply max r1)
                       (apply max r2))])
    [:lit n] [n]
    [:inp _] [1 9]))

(defn simplify
  [expr]
  (->> expr
       (walk/postwalk
         (fn [op]
           (match op
             [:add [:lit 0] [:lit 0]] [:lit 0]
             [:add [:lit n1] [:lit n2]] [:lit (+ n1 n2)]
             [:add [:add [:lit n1] exp] [:lit n2]] [:add exp [:lit (+ n1 n2)]]
             [:add [:add exp [:lit n1]] [:lit n2]] [:add exp [:lit (+ n1 n2)]]
             [:add [:lit n1] [:add [:lit n2] exp]] [:add exp [:lit (+ n1 n2)]]
             [:add [:lit n1] [:add exp [:lit n2]]] [:add exp [:lit (+ n1 n2)]]
             [:add [:lit 0] exp] exp
             [:add exp [:lit 0]] exp
             [:mul [:lit 0] _] [:lit 0]
             [:mul _ [:lit 0]] [:lit 0]
             [:mul [:lit 1] exp] exp
             [:mul exp [:lit 1]] exp
             [:mul [:lit n1] [:lit n2]] [:lit (* n1 n2)]
             [:div exp [:lit 1]] exp
             [:div [:lit 0] _] [:lit 0]
             [:div [:lit n1] [:lit n2]] [:lit (quot n1 n2)]
             [:mod [:lit 0] exp] [:lit 0]
             [:mod [:lit n1] [:lit n2]] [:lit (rem n1 n2)]
             [:mod exp [:lit n]] (if (< (first (compute-range exp)) n)
                                   exp
                                   op)
             [:eql [:inp _] [:lit (n :guard #(or (> % 9) (< % 1)))]] [:lit 0]
             [:eql [:lit (n :guard #(or (> % 9) (< % 1)))] [:inp _]] [:lit 0]
             [:eql [:lit n1] [:lit n2]] [:lit (if (== n1 n2) 1 0)]
             :else op)))))

(defn part1
  [input]
  (->> (to-expr input)
       simplify))

(defn part2
  [input]
  (count (part1 input)))
