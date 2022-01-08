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
     [:emit expr] (let [r (symbol (str "r-" (:counter state)))]
                    [r (-> state
                           (update :counter inc)
                           (update :bindings concat
                                   [r expr]))])
     [:emit-2 expr] (let [r1 (symbol (str "r-" (+ 0 (:counter state))))
                          r2 (symbol (str "r-" (+ 1 (:counter state))))
                          r3 (symbol (str "r-" (+ 2 (:counter state))))]
                      [[r2 r3]
                       (-> state
                           (update :counter + 3)
                           (update :bindings concat
                                   [r1 expr r2 `(nth ~r1 0) r3 `(nth ~r1 1)]))]))))

(defn compile-expr
  [expr]
  (let [h (fn rec [expr]
            (match expr
              [:reg r] [:emit-2 `(get ~'state ~r)]
              [:inp] [:emit-2 'input]
              [:lit n] [:emit-2 [n n]]
              [:add e1 e2] (mdo [[m1 M1] (rec e1)
                                 [m2 M2] (rec e2)
                                 _ [:emit-2 [`(+ ~m1 ~m2)
                                             `(+ ~M1 ~M2)]]])
              [:mul e1 e2] (mdo [[m1 M1] (rec e1)
                                 [m2 M2] (rec e2)
                                 a [:emit `(* ~m1 ~m2)]
                                 b [:emit `(* ~m1 ~M2)]
                                 c [:emit `(* ~M1 ~m2)]
                                 d [:emit `(* ~M1 ~M2)]
                                 r1 [:emit `(min ~a ~b ~c ~d)]
                                 r2 [:emit `(max ~a ~b ~c ~d)]
                                 _ [:emit-2 [r1 r2]]])
              [:div e [:lit n]] (mdo [[m M] (rec e)
                                      _ [:emit-2 `(sort [(quot ~m ~n) (quot ~M ~n)])]])
              [:mod e [:lit n]] (mdo [[m M] (rec e)
                                      _ [:emit-2 `(if (or (> (- ~M ~m) ~n)
                                                          (> (rem ~m ~n) (rem ~M ~n)))
                                                    [0 (dec ~n)]
                                                    [(rem ~m ~n) (rem ~M ~n)])]])
              [:eql e1 e2] (mdo [[m1 M1] (rec e1)
                                 [m2 M2] (rec e2)
                                 _ [:emit-2 `(cond (< ~M2 ~m1) [0 0]
                                                   (< ~M1 ~m2) [0 0]
                                                   (= ~m1 ~M1 ~m2 ~M2) [1 1]
                                                   :else [0 1])]])))
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
     (map :z))
([:add [:mul [:reg :z] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 10]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 2]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 10]] [:inp]] [:lit 0]]]]

 [:add [:mul [:reg :z] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 15]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 16]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 15]] [:inp]] [:lit 0]]]]

 [:add [:mul [:reg :z] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 14]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 9]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 14]] [:inp]] [:lit 0]]]]

 [:add [:mul [:reg :z] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 15]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:inp] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 15]] [:inp]] [:lit 0]]]]

 [:add [:mul [:div [:reg :z] [:lit 26]] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -8]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 1]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -8]] [:inp]] [:lit 0]]]]

 [:add [:mul [:reg :z] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 10]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 12]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 10]] [:inp]] [:lit 0]]]]

 [:add [:mul [:div [:reg :z] [:lit 26]] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -16]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 6]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -16]] [:inp]] [:lit 0]]]]

 [:add [:mul [:div [:reg :z] [:lit 26]] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -4]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 6]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -4]] [:inp]] [:lit 0]]]]

 [:add [:mul [:reg :z] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 11]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 3]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 11]] [:inp]] [:lit 0]]]]

 [:add [:mul [:div [:reg :z] [:lit 26]] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -3]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 5]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -3]] [:inp]] [:lit 0]]]]

 [:add [:mul [:reg :z] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 12]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 9]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit 12]] [:inp]] [:lit 0]]]]

 [:add [:mul [:div [:reg :z] [:lit 26]] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -7]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 3]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -7]] [:inp]] [:lit 0]]]]

  [:add [:mul [:div [:reg :z] [:lit 26]] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -15]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 2]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -15]] [:inp]] [:lit 0]]]]

  )

(def step [:add [:mul [:div [:reg :z] [:lit 26]] [:add [:mul [:lit 25] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -7]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:inp] [:lit 3]] [:eql [:eql [:add [:mod [:reg :z] [:lit 26]] [:lit -7]] [:inp]] [:lit 0]]]])

(compute-range-expr step [1 9] init-state)
[4 12]

(compile-expr step)

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
