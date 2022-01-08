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

(defn compile-instr
  [instr]
  (let [state (gensym "state")
        input (gensym "input")
        i (gensym "i")
        I (gensym "I")
        w (gensym "w")
        W (gensym "W")
        x (gensym "x")
        X (gensym "X")
        y (gensym "y")
        Y (gensym "Y")
        z (gensym "z")
        Z (gensym "Z")
        t1 (gensym)
        t2 (gensym)
        t3 (gensym)
        t4 (gensym)
        r (fn [r n]
            (get-in {:w [w W]
                     :x [x X]
                     :y [y Y]
                     :z [z Z]} [r n]))]
    (eval `(fn [~state ~input]
       (let [~i (get ~input 0)
             ~I (get ~input 1)
             ~w (get-in ~state [:w 0])
             ~W (get-in ~state [:w 1])
             ~x (get-in ~state [:x 0])
             ~X (get-in ~state [:x 1])
             ~y (get-in ~state [:y 0])
             ~Y (get-in ~state [:y 1])
             ~z (get-in ~state [:z 0])
             ~Z (get-in ~state [:z 1])
             ~@(->> instr
                    (mapcat
                      (fn [op]
                        (match op
                          [:inp r1] [(r r1 0) i (r r1 1) I]
                          [:add _ [:lit 0]] []
                          [:add r1 [:lit n]] [(r r1 0) `(+ ~(r r1 0) ~n)
                                              (r r1 1) `(+ ~(r r1 1) ~n)]
                          [:add r1 [:reg r2]] [(r r1 0) `(+ ~(r r1 0) ~(r r2 0))
                                               (r r1 1) `(+ ~(r r1 1) ~(r r2 1))]
                          [:mul r1 [:lit 0]] [(r r1 0) 0
                                              (r r1 1) 0]
                          [:mul r1 [:lit 1]] []
                          [:mul r1 [:lit n]] [(r r1 0) `(* ~(r r1 0) ~n)
                                              (r r1 1) `(* ~(r r1 1) ~n)]
                          [:mul r1 [:reg r2]] [t1 `(* ~(r r1 0) ~(r r2 0))
                                               t2 `(* ~(r r1 0) ~(r r2 1))
                                               t3 `(* ~(r r1 1) ~(r r2 0))
                                               t4 `(* ~(r r1 1) ~(r r2 1))
                                               (r r1 0) `(min ~t1 ~t2 ~t3 ~t4)
                                               (r r1 1) `(max ~t1 ~t2 ~t3 ~t4)]
                          [:div r1 [:lit 1]] []
                          [:div r1 [:lit n]] [(r r1 0) `(quot ~(r r1 0) ~n)
                                              (r r1 1) `(quot ~(r r1 1) ~n)]
                          [:mod r1 [:lit n]] [t1 `(or (> (- ~(r r1 1) ~(r r1 0)) ~n)
                                                      (> (rem ~(r r1 0) ~n)
                                                         (rem ~(r r1 1) ~n)))
                                              (r r1 0) `(if ~t1 0 (rem ~(r r1 0) ~n))
                                              (r r1 1) `(if ~t1 (dec ~n) (rem ~(r r1 1) ~n))]
                          [:eql r1 [:lit 0]] []
                          [:eql r1 [:reg r2]] [t1 `(= ~(r r1 0) ~(r r1 1) ~(r r2 0) ~(r r2 1))
                                               t2 `(or (< ~(r r2 1) ~(r r1 0))
                                                       (< ~(r r1 1) ~(r r2 0)))
                                               (r r1 0) `(if ~t2 1 0)
                                               (r r1 1) `(if ~t1 0 1)]))))]
         {:w [~w ~W] :x [~x ~X] :y [~y ~Y] :z [~z ~Z]})))))

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
  [bindings body]
  (if (== 1 (count bindings))
    (throw
      (RuntimeException. "invalid number of elements in mdo bindings"))
    (if (empty? bindings)
      [:return body]
      (let [[n mv & bindings] bindings]
        [:bind mv `(fn [~n] (mdo ~bindings ~body))]))))

(defn to-bindings
  ([m] (to-bindings m {:bindings (), :rbindings {}}))
  ([m state]
   (match m
     [:return v] [v state]
     [:bind ma f] (let [[v state] (to-bindings ma state)]
                    (to-bindings (f v) state))
     [:expr expr] (if-let [sym  (get (:rbindings state) expr)]
                    [sym state]
                    (let [sym (symbol (str "r-" (count (:rbindings state))))]
                      [sym (-> state
                               (update :bindings concat [sym expr])
                               (update :rbindings assoc expr sym))])))))

(defn compile-expr
  [expr]
  (let [h (fn rec [expr]
            (match expr
              [:reg r] (mdo [s1 [:expr `(get ~'state ~r)]
                             s2 [:expr `(get ~s1 0)]
                             s3 [:expr `(get ~s1 1)]]
                         [s2 s3])
              [:inp] (mdo [m [:expr `(get ~'input 0)]
                           M [:expr `(get ~'input 1)]]
                       [m M])
              [:lit n] (mdo []
                         [n n])
              [:add e1 e2] (mdo [[m1 M1] (rec e1)
                                 [m2 M2] (rec e2)
                                 s1 [:expr `(+ ~m1 ~m2)]
                                 s2 [:expr `(+ ~M1 ~M2)]]
                             [s1 s2])
              [:mul e1 e2] (mdo [[m1 M1] (rec e1)
                                 [m2 M2] (rec e2)
                                 r1 [:expr `(* ~m1 ~m2)]
                                 r2 [:expr `(* ~M1 ~M2)]]
                             [r1 r2])
              [:div e [:lit n]] (mdo [[m M] (rec e)
                                      s1 [:expr `(quot ~m ~n)]
                                      s2 [:expr `(quot ~M ~n)]]
                                  [s1 s2])
              [:mod e [:lit n]] (mdo [[m M] (rec e)
                                      s1 [:expr `(or (> (- ~M ~m) ~n)
                                                     (> (rem ~m ~n) (rem ~M ~n)))]
                                      s2 [:expr `(if ~s1 0 (rem ~m ~n))]
                                      s3 [:expr `(if ~s1 (dec ~n) (rem ~M ~n))]]
                                  [s2 s3])
              [:eqn e1 e2] (mdo [[m1 M1] (rec e1)
                                 [m2 M2] (rec e2)
                                 s1 [:expr `(= ~m1 ~M1 ~m2 ~M2)]
                                 s2 [:expr `(or (< ~M2 ~m1)
                                                (< ~M1 ~m2))]
                                 s3 [:expr `(if ~s2 1 0)]
                                 s4 [:expr `(if ~s1 0 1)]]
                             [s3 s4])))
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


(compile-expr step)
  (let [state init-state
        input [1 9]
        r-0 (get state :z)
        r-1 (get r-0 0)
        r-2 (get r-0 1)
        r-3 (quot r-1 26)
        r-4 (quot r-2 26)
        r-5 (min r-3 r-4)
        r-6 (max r-3 r-4)
        r-7 (or (> (- r-2 r-1) 26) (> (rem r-1 26) (rem r-2 26)))
        r-8 (if r-7 0 (rem r-1 26))
        r-9 (if r-7 (dec 26) (rem r-2 26))
        r-10 (+ r-8 -7)
        r-11 (+ r-9 -7)
        r-12 (get input 0)
        r-13 (get input 1)
        r-14 (= r-10 r-11 r-12 r-13)
        r-15 (or (< r-13 r-10) (< r-11 r-12))
        r-16 (if r-14 1 0)
        r-17 (if r-15 0 1)
        r-18 (= r-16 r-17 0 0)
        r-19 (or (< 0 r-16) (< r-17 0))
        r-20 (if r-18 1 0)
        r-21 (if r-19 0 1)
        r-22 (* 25 r-20)
        r-23 (* 25 r-21)
        r-24 (min r-22 r-23 r-22 r-23)
        r-25 (max r-22 r-23 r-22 r-23)
        r-26 (+ r-24 1)
        r-27 (+ r-25 1)
        r-28 (* r-3 r-26)
        r-29 (* r-3 r-27)
        r-30 (* r-4 r-26)
        r-31 (* r-4 r-27)
        r-32 (min r-28 r-29 r-30 r-31)
        r-33 (max r-28 r-29 r-30 r-31)
        r-34 (+ r-12 3)
        r-35 (+ r-13 3)
        r-36 (* r-34 r-20)
        r-37 (* r-34 r-21)
        r-38 (* r-35 r-20)
        r-39 (* r-35 r-21)
        r-40 (min r-36 r-37 r-38 r-39)
        r-41 (max r-36 r-37 r-38 r-39)
        r-42 (+ r-32 r-40)
        r-43 (+ r-33 r-41)]
    [r-42 r-43])




[4 12]

(->> input
     (partition-by (fn [[op arg]] (= op :inp)))
     (partition 2)
     (map (fn [[input ops]] (concat input ops)))
     last
     compile-instr)

  (let [state14006 init-state
        input14007 [1 9]
        i14008 (get input14007 0)
        I14009 (get input14007 1)
        w14010 (get-in state14006 [:w 0])
        W14011 (get-in state14006 [:w 1])
        x14012 (get-in state14006 [:x 0])
        X14013 (get-in state14006 [:x 1])
        y14014 (get-in state14006 [:y 0])
        Y14015 (get-in state14006 [:y 1])
        z14016 (get-in state14006 [:z 0])
        Z14017 (get-in state14006 [:z 1])
        w14010 i14008
        W14011 I14009
        x14012 0
        X14013 0
        x14012 (+ x14012 z14016)
        X14013 (+ X14013 Z14017)
        G__14018 (or (> (- X14013 x14012) 26) (> (rem x14012 26) (rem X14013 26)))
        x14012 (if G__14018 0 (rem x14012 26))
        X14013 (if G__14018 (dec 26) (rem X14013 26))
        z14016 (quot z14016 26)
        Z14017 (quot Z14017 26)
        x14012 (+ x14012 -7)
        X14013 (+ X14013 -7)
        G__14018 (= x14012 X14013 w14010 W14011)
        G__14019 (or (< W14011 x14012) (< X14013 w14010))
        x14012 (if G__14019 1 0)
        X14013 (if G__14018 0 1)
        y14014 0
        Y14015 0
        y14014 (+ y14014 25)
        Y14015 (+ Y14015 25)
        G__14018 (* y14014 x14012)
        G__14019 (* y14014 X14013)
        G__14020 (* Y14015 x14012)
        G__14021 (* Y14015 X14013)
        y14014 (min G__14018 G__14019 G__14020 G__14021)
        Y14015 (max G__14018 G__14019 G__14020 G__14021)
        y14014 (+ y14014 1)
        Y14015 (+ Y14015 1)
        G__14018 (* z14016 y14014)
        G__14019 (* z14016 Y14015)
        G__14020 (* Z14017 y14014)
        G__14021 (* Z14017 Y14015)
        z14016 (min G__14018 G__14019 G__14020 G__14021)
        Z14017 (max G__14018 G__14019 G__14020 G__14021)
        y14014 0
        Y14015 0
        y14014 (+ y14014 w14010)
        Y14015 (+ Y14015 W14011)
        y14014 (+ y14014 3)
        Y14015 (+ Y14015 3)
        G__14018 (* y14014 x14012)
        G__14019 (* y14014 X14013)
        G__14020 (* Y14015 x14012)
        G__14021 (* Y14015 X14013)
        y14014 (min G__14018 G__14019 G__14020 G__14021)
        Y14015 (max G__14018 G__14019 G__14020 G__14021)
        z14016 (+ z14016 y14014)
        Z14017 (+ Z14017 Y14015)]
    [w14010
     W14011
     x14012
     X14013
     y14014
     Y14015
     z14016
     Z14017])

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
                         :else [0 1]))
    [:eqn e1 e2] [0 1]
    :else exp))

(def init-state
  {:w [0 0], :x [0 0], :y [0 0], :z [0 0]})

(defn solve-instr
  [instr target reverse?]
  (let [split-exprs (->> instr
                         (partition-by (fn [[op arg]] (= op :inp)))
                         (partition 2)
                         (map (fn [[input ops]] (concat input ops)))
                         (map compile-instr))
        h (fn rec [state exprs input-so-far]
            (let [[m M] (:z (reduce (fn [state f]
                                      (f state [1 9]))
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
                                 (rec ((first exprs) state [next-input next-input])
                                      (rest exprs)
                                      (+ (* 10 input-so-far) next-input))))))))]
    (h init-state split-exprs 0)))

(defn solve-expr
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
  (solve-expr input 0 true))

(defn part2
  [input]
  (solve-expr input 0 false))
