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
  (let [c (atom 0)
        h (fn rec [fixed-input]
            (let [input (take size (concat fixed-input (repeat [1 9])))
                  [m M] (compute-range instr input)]
              (swap! c inc)
              (when (zero? (rem @c 10000))
                (prn [input m M]))
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
