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
({:w [:inp], :x [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 10]] [:inp]] [:lit 0]], :y [:mul [:add [:add [:lit 0] [:inp]] [:lit 2]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 10]] [:inp]] [:lit 0]]], :z [:add [:mul [:reg :z] [:add [:mul [:add [:lit 0] [:lit 25]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 10]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:add [:lit 0] [:inp]] [:lit 2]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 10]] [:inp]] [:lit 0]]]]}
 {:w [:inp], :x [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 15]] [:inp]] [:lit 0]], :y [:mul [:add [:add [:lit 0] [:inp]] [:lit 16]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 15]] [:inp]] [:lit 0]]], :z [:add [:mul [:reg :z] [:add [:mul [:add [:lit 0] [:lit 25]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 15]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:add [:lit 0] [:inp]] [:lit 16]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 15]] [:inp]] [:lit 0]]]]}
 {:w [:inp], :x [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 14]] [:inp]] [:lit 0]], :y [:mul [:add [:add [:lit 0] [:inp]] [:lit 9]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 14]] [:inp]] [:lit 0]]], :z [:add [:mul [:reg :z] [:add [:mul [:add [:lit 0] [:lit 25]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 14]] [:inp]] [:lit 0]]] [:lit 1]]] [:mul [:add [:add [:lit 0] [:inp]] [:lit 9]] [:eql [:eql [:add [:mod [:add [:lit 0] [:reg :z]] [:lit 26]] [:lit 14]] [:inp]] [:lit 0]]]]})

  )

(defn compute-range
  [instr inputs]
  (loop [instr instr
         inputs inputs
         state {:w [0 0], :x [0 0], :y [0 0], :z [0 0]}]
    (if (empty? instr)
      (:z state)
      (let [op (first instr)]
        (if (= op [:inp :w])
          (recur (rest instr)
                 (rest inputs)
                 (assoc state :w (first inputs)))
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

(defn solve
  [instr size target reverse?]
  (let [h (fn rec [fixed-input]
            (let [input (take size (concat fixed-input (repeat [1 9])))
                  [m M] (compute-range instr input)]
              (cond (and (= (count fixed-input) size)
                         (== m M target))
                    (->> fixed-input (map first) (apply str) Long/parseLong)
                    (or (= (count fixed-input) size)
                        (not (<= m target M)))
                    nil
                    :else
                    (->> (range 9)
                         (map inc)
                         ((fn [s] (if reverse? (reverse s) s)))
                         (map (fn [n] (conj fixed-input [n n])))
                         (some rec)))))]
    (h [])))

(defn part1
  [input]
  (let [input-size (->> input (map first) (filter #{:inp}) count)
        all-inputs (repeat input-size [1 9])
        target (first (compute-range input all-inputs))]
    (solve input input-size target true)))

(defn part2
  [input]
  (let [input-size (->> input (map first) (filter #{:inp}) count)
        all-inputs (repeat input-size [1 9])
        target (first (compute-range input all-inputs))]
    (solve input input-size target false)))
