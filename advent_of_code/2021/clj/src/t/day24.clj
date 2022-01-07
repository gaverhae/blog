(ns t.day24
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]
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

(defn to-exprs
  [instrs]
  (->> instrs
       (partition-by (fn [[op arg]] (= op :inp)))
       (partition 2)
       (map (fn [[input ops]] (concat input ops)))
       (map
         (fn [ops]
           (reduce (fn [acc instr]
                     (match instr
                       [:inp r] (assoc acc r [:inp])
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
                   {:w [:reg :w], :x [:reg :x], :y [:reg :y], :z [:reg :z]}
                   ops)))
       #_(map (fn [exprs]
              (assoc exprs :read (->> exprs
                                      (map val)
                                      (map (fn rec [v]
                                             (match v
                                               (:or [:inp]
                                                    [:lit _])
                                               #{}
                                               (:or [:add arg1 arg2]
                                                    [:mul arg1 arg2]
                                                    [:div arg1 arg2]
                                                    [:mod arg1 arg2]
                                                    [:eql arg1 arg2])
                                               (set/union (rec arg1) (rec arg2))
                                               [:reg r] #{r})))
                                      (reduce set/union)))))
       #_reverse
       #_(reduce (fn [[read-from-prev exprs] step]
                 [(:read step)
                  (cons (select-keys step read-from-prev) exprs)])
               [#{:z} ()])
       #_second
       #_(map (fn [m]
              (->> m
                   (map (fn [[k v]]
                          [k
                           (walk/postwalk
                             (fn [op] (match op
                                        [:add [:lit 0] exp] exp
                                        [:add exp [:lit 0]] exp
                                        :else op))
                             v)]))
                   (into {}))))))

(defn compute-range-expr
  [exp input state]
  (match exp
    [:reg r] (r state)
    [:inp] input
    [:lit n] [n n]
    [:add e1 e2] (let [[m1 M1] (compute-range-expr e1 input state)
                       [m2 M2] (compute-range-expr e2 input state)]
                   [(+ m1 m2) (+ M1 M2)])
    [:mul e1 e2] (let [[m1 M1] (compute-range-expr e1 input state)
                       [m2 M2] (compute-range-expr e2 input state)
                       prods (for [f1 [m1 M1]
                                   f2 [m2 M2]]
                               (* f1 f2))]
                   [(apply min prods)
                    (apply max prods)])
    [:div e [:lit n]] (let [[m M] (compute-range-expr e input state)]
                        (sort [(quot m n) (quot M n)]))
    [:mod e1 [:lit n]] (let [[m M] (compute-range-expr e1 input state)]
                             (if (or (> (- M m) n)
                                     (> (rem m n) (rem M n)))
                               [0 (dec n)]
                               [(rem m n) (rem M n)]))
    [:eql e1 e2] (let [[m1 M1] (compute-range-expr e1 input state)
                       [m2 M2] (compute-range-expr e2 input state)]
                   (cond (< M2 m1) [0 0]
                         (< M1 m2) [0 0]
                         (= m1 M1 m2 M2) [1 1]
                         :else [0 1]))))

(def init-state
  {:w [0 0], :x [0 0], :y [0 0], :z [0 0]})

(defn solve
  [instr size target reverse?]
  (let [split-exprs (to-exprs instr)
        h (fn rec [state exprs input-so-far]
            (let [[m M] (:z (reduce (fn [state expr]
                                      (->> expr
                                           (map (fn [[reg expr]]
                                                  [reg (compute-range-expr expr [1 9] state)]))
                                           (into {})))
                                    state
                                    exprs))]
              (cond (and (empty? exprs) (== target m M))
                    input-so-far
                    (or (empty? exprs) (not (<= m target M)))
                    nil
                    :else
                    (->> (range 9)
                         (map inc)
                         ((fn [s] (if reverse? (reverse s) s)))
                         (some (fn [next-input]
                                 (rec (->> (first exprs)
                                           (map (fn [[reg expr]]
                                                  [reg (compute-range-expr expr [next-input next-input] state)]))
                                           (into {}))
                                      (rest exprs)
                                      (+ (* 10 input-so-far) next-input))))))))]
    (h init-state split-exprs 0)))

(defn part1
  [input]
  (let [input-size (->> input (map first) (filter #{:inp}) count)]
    (solve input input-size 0 true)))

(defn part2
  [input]
  (let [input-size (->> input (map first) (filter #{:inp}) count)]
    (solve input input-size 0 false)))
