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
                   ops)))))

(defn remove-unneeded-registers
  [exprs]
  (->> exprs
       (map (fn [exprs]
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
       reverse
       (reduce (fn [[read-from-prev exprs] step]
                 [(:read step)
                  (cons (select-keys step read-from-prev) exprs)])
               [#{:z} ()])
       second))

(defn simplify-expr
  [expr]
  (walk/postwalk
    (fn [op] (match op
               [:add [:lit 0] exp] exp
               [:add exp [:lit 0]] exp
               [:eql [:eql e1 e2] [:lit 0]] [:eqn e1 e2]
               [:eql [:lit 0] [:eql e1 e2]] [:eqn e1 e2]
               :else op))
    expr))

(defmacro mdo
  [bindings]
  (if (#{0 1} (count bindings))
    (throw
      (RuntimeException. "invalid number of elements in mdo bindings"))
    (let [[n v & r] bindings]
      (if (empty? r)
        v
        [:bind v `(fn [~n] (mdo ~r))]))))

(defn to-bindings
  ([m] (to-bindings m {:bindings [], :counter 0}))
  ([m state]
   (match m
     [:return v] [v state]
     [:bind ma f] (let [[v state] (to-bindings ma state)]
                    (to-bindings (f v) state))
     [:emit expr] [nil (update state :bindings concat expr)]
     [:get-symbol] (let [sym (symbol (str "r-" (:counter state)))]
                     [sym (update state :counter inc)]))))

(defn compile-expr
  [expr]
  (let [h (fn rec [expr]
            (match expr
              [:reg r] (mdo [s1 [:get-symbol]
                             s2 [:get-symbol]
                             s3 [:get-symbol]
                             _ [:emit [s1 `(get ~'state ~r)
                                       s2 `(get ~s1 0)
                                       s3 `(get ~s1 1)]]
                             _ [:return [s2 s3]]])
              [:inp] (mdo [m [:get-symbol]
                           M [:get-symbol]
                           _ [:emit [m `(get ~'input 0)
                                     M `(get ~'input 1)]]
                           _ [:return [m M]]])
              [:lit n] (mdo [_ [:return [n n]]])
              [:add e1 e2] (mdo [[m1 M1] (rec e1)
                                 [m2 M2] (rec e2)
                                 s1 [:get-symbol]
                                 s2 [:get-symbol]
                                 _ [:emit [s1 `(+ ~m1 ~m2)
                                           s2 `(+ ~M1 ~M2)]]
                                 _ [:return [s1 s2]]])
              [:mul e1 e2] (mdo [[m1 M1] (rec e1)
                                 [m2 M2] (rec e2)
                                 a [:get-symbol]
                                 b [:get-symbol]
                                 c [:get-symbol]
                                 d [:get-symbol]
                                 r1 [:get-symbol]
                                 r2 [:get-symbol]
                                 _ [:emit [a `(* ~m1 ~m2)
                                           b `(* ~m1 ~M2)
                                           c `(* ~M1 ~m2)
                                           d `(* ~M1 ~M2)
                                           r1 `(min ~a ~b ~c ~d)
                                           r2 `(max ~a ~b ~c ~d)]]
                                 _ [:return [r1 r2]]])
              [:div e [:lit n]] (mdo [[m M] (rec e)
                                      s1 [:get-symbol]
                                      s2 [:get-symbol]
                                      s3 [:get-symbol]
                                      s4 [:get-symbol]
                                      _ [:emit [s1 `(quot ~m ~n)
                                                s2 `(quot ~M ~n)
                                                s3 `(min ~s1 ~s2)
                                                s4 `(max ~s1 ~s2)]]
                                      _ [:return [s1 s2]]])
              [:mod e [:lit n]] (mdo [[m M] (rec e)
                                      s1 [:get-symbol]
                                      s2 [:get-symbol]
                                      s3 [:get-symbol]
                                      _ [:emit [s1 `(or (> (- ~M ~m) ~n)
                                                        (> (rem ~m ~n) (rem ~M ~n)))
                                                s2 `(if ~s1 0 (rem ~m ~n))
                                                s3 `(if ~s1 (dec ~n) (rem ~M ~n))]]
                                      _ [:return [s2 s3]]])
              [:eqn e1 e2] (mdo [[m1 M1] (rec e1)
                                 [m2 M2] (rec e2)
                                 s1 [:get-symbol]
                                 s2 [:get-symbol]
                                 s3 [:get-symbol]
                                 s4 [:get-symbol]
                                 _ [:emit [s1 `(= ~m1 ~M1 ~m2 ~M2)
                                           s2 `(or (< ~M2 ~m1)
                                                   (< ~M1 ~m2))
                                           s3 `(if ~s2 1 0)
                                           s4 `(if ~s1 0 1)]]
                                 _ [:return [s3 s4]]])
              [:eql e1 e2] (mdo [[m1 M1] (rec e1)
                                 [m2 M2] (rec e2)
                                 s1 [:get-symbol]
                                 s2 [:get-symbol]
                                 s3 [:get-symbol]
                                 s4 [:get-symbol]
                                 _ [:emit [s1 `(= ~m1 ~M1 ~m2 ~M2)
                                           s2 `(or (< ~M2 ~m1)
                                                   (< ~M1 ~m2))
                                           s3 `(if ~s1 1 0)
                                           s4 `(if ~s2 0 1)]]
                                 _ [:return [s3 s4]]])))
        [result {:keys [bindings]}] (to-bindings (h expr))]
    (eval `(fn [~'state ~'input]
             (let [~@bindings]
               ~result)))))

(comment

(def input (parse (clojure.string/split-lines (slurp "data/day24"))))

(->> input
     to-exprs
     remove-unneeded-registers
     simplify-expr
     (map :z)
     (map compile-expr)
     (map (fn [f]
            (f init-state [1 9]))))
([3 11] [17 25] [10 18] [1 9] [2 10] [13 21] [7 15] [7 15] [4 12] [6 14] [10 18] [4 12] [3 11] [4 12])

(def step [:add [:mul [:div [:reg :z] [:lit 26]] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -7]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 3]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -7]] [:inp]] [:lit 0]]]])

(compute-range-expr step [1 9] init-state)
[4 12]

((compile-expr step) init-state [1 9])

((eval (compile-expr step)) init-state [1 9])
[4 12]





  )

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
  [instr target reverse?]
  (let [split-exprs (->> instr
                         to-exprs
                         remove-unneeded-registers
                         (map (fn [step]
                                (->> step
                                     (map (fn [[reg expr]]
                                            [reg (-> expr
                                                     simplify-expr
                                                     compile-expr)]))
                                     (into {})))))
        h (fn rec [state exprs input-so-far]
            (let [[m M] (:z (reduce (fn [state expr]
                                      (->> expr
                                           (map (fn [[reg f]]
                                                  [reg (f state [1 9])]))
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
                                           (map (fn [[reg f]]
                                                  [reg (f state [next-input next-input])]))
                                           (into {}))
                                      (rest exprs)
                                      (+ (* 10 input-so-far) next-input))))))))]
    (h init-state split-exprs 0)))

(defn part1
  [input]
  (solve input 0 true))

(defn part2
  [input]
  (solve input 0 false))
