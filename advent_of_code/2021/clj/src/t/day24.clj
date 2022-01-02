(ns t.day24
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]))

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

(comment

  (def sample
    [[:inp :w] [:mul :x [:lit 0]] [:add :x [:reg :z]] [:mod :x [:lit 26]]
     [:div :z [:lit 1]] [:add :x [:lit 10]] [:eql :x [:reg :w]] [:eql :x [:lit 0]]
     [:mul :y [:lit 0]] [:add :y [:lit 25]] [:mul :y [:reg :x]] [:add :y [:lit 1]]
     [:mul :z [:reg :y]] [:mul :y [:lit 0]] [:add :y [:reg :w]] [:add :y [:lit 2]]
     [:mul :y [:reg :x]] [:add :z [:reg :y]]
     [:inp :w] [:mul :x [:lit 0]] [:add :x [:reg :z]] [:mod :x [:lit 26]]
     [:div :z [:lit 1]] [:add :x [:lit 15]] [:eql :x [:reg :w]] [:eql :x [:lit 0]]
     [:mul :y [:lit 0]] [:add :y [:lit 25]] [:mul :y [:reg :x]] [:add :y [:lit 1]]
     [:mul :z [:reg :y]] [:mul :y [:lit 0]] [:add :y [:reg :w]] [:add :y [:lit 16]]
     [:mul :y [:reg :x]] [:add :z [:reg :y]]
     [:inp :w] [:mul :x [:lit 0]] [:add :x [:reg :z]] [:mod :x [:lit 26]]
     [:div :z [:lit 1]] [:add :x [:lit 14]] [:eql :x [:reg :w]] [:eql :x [:lit 0]]
     [:mul :y [:lit 0]] [:add :y [:lit 25]] [:mul :y [:reg :x]] [:add :y [:lit 1]]
     [:mul :z [:reg :y]] [:mul :y [:lit 0]] [:add :y [:reg :w]] [:add :y [:lit 9]]
     [:mul :y [:reg :x]] [:add :z [:reg :y]]])

  (to-exprs sample)
({:z [:add [:mul [:reg :z] [:add [:mul [:add [:lit 0] [:lit 25]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 10]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:add [:lit 0] [:inp]] [:lit 2]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 10]] [:inp]] [:lit 0]]]]}
 {:z [:add [:mul [:reg :z] [:add [:mul [:add [:lit 0] [:lit 25]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 15]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:add [:lit 0] [:inp]] [:lit 16]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 15]] [:inp]] [:lit 0]]]]}
 {:z [:add [:mul [:reg :z] [:add [:mul [:add [:lit 0] [:lit 25]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 14]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:add [:lit 0] [:inp]] [:lit 9]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 14]] [:inp]] [:lit 0]]]]})

  )

(defn compute-range
  [instr inputs state]
  (loop [instr instr
         inputs inputs
         state state]
    (if (empty? instr)
      state
      (let [op (first instr)]
        (if (= (first op) :inp)
          (recur (rest instr)
                 (rest inputs)
                 (assoc state (second op) (first inputs)))
          (recur (rest instr)
                 inputs
                 (match op
                   [:add r [:lit n]] (update state r (fn [[m M]] [(+ m n) (+ M n)]))
                   [:add r1 [:reg r2]] (update state r1 (fn [[m1 M1]]
                                                          (let [[m2 M2] (get state r2)]
                                                            [(+ m1 m2) (+ M1 M2)])))
                   [:mul r [:lit n]] (update state r (fn [[m M]]
                                                       (sort [(* m n) (* M n)])))
                   [:mul r1 [:reg r2]] (update state r1 (fn [[m1 M1]]
                                                          (let [[m2 M2] (get state r2)
                                                                prods (for [m [m1 M1]
                                                                            n [m2 M2]]
                                                                        (* m n))]
                                                            [(apply min prods)
                                                             (apply max prods)])))
                   [:div r [:lit n]] (update state r (fn [[m M]]
                                                       (sort [(quot m n) (quot M n)])))
                   [:mod r [:lit n]] (update state r (fn [[m M]]
                                                       (if (or (> (- M m) n)
                                                               (> (rem m n) (rem M n)))
                                                         [0 (dec n)]
                                                         [(rem m n) (rem M n)])))
                   [:eql r [:lit n]] (update state r (fn [[m M]]
                                                       (cond (= m n M) [1 1]
                                                             (<= m n M) [0 1]
                                                             :else [0 0])))
                   [:eql r1 [:reg r2]] (update state r1 (fn [[m1 M1]]
                                                          (let [[m2 M2] (get state r2)]
                                                            (cond (< M2 m1) [0 0]
                                                                  (< M1 m2) [0 0]
                                                                  (= m1 M1 m2 M2) [1 1]
                                                                  :else [0 1])))))))))))

(def init-state
  {:w [0 0], :x [0 0], :y [0 0], :z [0 0]})

(defn solve
  [instr size target reverse?]
  (let [split-instrs (->> instr
                          (partition-by (fn [[op arg]] (= op :inp)))
                          (partition 2)
                          (map (fn [[[input] ops]] (cons input ops))))
        h (fn rec [state instrs input-so-far]
            (let [input (repeat [1 9])
                  [m M] (:z (compute-range (apply concat instrs) input state))]
              (cond (and (empty? instrs) (== target m M))
                    input-so-far
                    (or (empty? instrs) (not (<= m target M)))
                    nil
                    :else
                    (->> (range 9)
                         (map inc)
                         ((fn [s] (if reverse? (reverse s) s)))
                         (some (fn [next-input]
                                 (rec (compute-range (first instrs)
                                                     [[next-input next-input]]
                                                     state)
                                      (rest instrs)
                                      (+ (* 10 input-so-far) next-input))))))))]
    (h init-state split-instrs 0)))

(defn part1
  [input]
  (let [input-size (->> input (map first) (filter #{:inp}) count)
        all-inputs (repeat [1 9])
        target (first (:z (compute-range input all-inputs init-state)))]
    (solve input input-size target true)))

(defn part2
  [input]
  (let [input-size (->> input (map first) (filter #{:inp}) count)
        all-inputs (repeat [1 9])
        target (first (:z (compute-range input all-inputs init-state)))]
    (solve input input-size target false)))
