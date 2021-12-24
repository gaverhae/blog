(ns t.day24
  (:require [clojure.core.match :refer [match]]
            [taoensso.tufte :as tufte :refer (fnp p profile)]))

(defn parse
  [lines]
  (->> lines
       (map (fn [l] (let [[_ op arg1 _ arg2] (re-matches #"(...) (.)( (-?.+))?" l)]
                      [(keyword op) (keyword arg1) arg2])))
       (map (fn [[op arg1 arg2]]
              (if (= op :inp)
                [:inp arg1]
                [op arg1 (if (Character/isLetter ^Character (first arg2))
                           (keyword arg2)
                           (Long/parseLong arg2))])))))

(defn to-fns-opt
  [instr]
  (let [arr (with-meta (gensym "arr") {:tag "[J"})
        ret (with-meta (gensym "ret") {:tag "[J"})
        w (gensym "w")
        x (gensym "x")
        y (gensym "y")
        z (gensym "z")
        to-sym {:w w, :x x, :y y, :z z}
        hint-state (atom {})]
    (->> instr
         (partition-by #{[:inp :w]})
         (partition 2)
         (map-indexed (fn [i [_ ops]]
                (eval `(fnp ~(symbol (str "opti-" i)) [~arr in#]
                         (let [~w in#
                               ~x (aget ~arr 1)
                               ~y (aget ~arr 2)
                               ~z (aget ~arr 3)
                               ~ret (make-array Long/TYPE 4)
                               ~@(mapcat (fn [[i a1 a2]]
                                           (match [i (to-sym a1) (if (keyword? a2) (to-sym a2) a2)]
                                             [:add s1 0] []
                                             [:add s1 s2] [s1 (do (swap! hint-state assoc s1 false)
                                                                  `(unchecked-add ~s1 ~s2))]
                                             [:mul s1 0] [s1 (do (swap! hint-state assoc s1 true)
                                                                 0)]
                                             [:mul s1 1] []
                                             [:mul s1 s2] [s1 (do (swap! hint-state assoc s1 false)
                                                                         `(unchecked-multiply ~s1 ~s2))]
                                             [:div s1 1] []
                                             [:div s1 s2] [s1 (do (swap! hint-state assoc s1 false)
                                                                  `(quot ~s1 ~s2))]
                                             [:mod s1 s2] [s1 (do (swap! hint-state assoc s1 false)
                                                                         `(rem ~s1 ~s2))]
                                             [:eql s1 0] []
                                             [:eql s1 s2] [s1 (do (swap! hint-state assoc s1 true)
                                                                  `(if (== ~s1 ~s2) 0 1))]))
                                         ops)]
                           (aset ~ret 0 ~(if (@hint-state w)
                                           w
                                           (with-meta w {:tag "long"})))
                           (aset ~ret 1 ~(if (@hint-state x)
                                           x
                                           (with-meta x {:tag "long"})))
                           (aset ~ret 2 ~(if (@hint-state y)
                                           y
                                           (with-meta y {:tag "long"})))
                           (aset ~ret 3 ~(if (@hint-state z)
                                           z
                                           (with-meta z {:tag "long"})))
                           ~ret))))))))

(defn to-fns
  [instr]
  (let [op {:add +, :mul *, :div quot, :mod mod, :eql (fn [a b] (if (== a b) 1 0))}
        arr (with-meta (gensym "arr") {:tag "[J"})
        ret (with-meta (gensym "ret") {:tag "[J"})
        w (gensym "w")
        x (gensym "x")
        y (gensym "y")
        z (gensym "z")
        to-sym {:w w, :x x, :y y, :z z}]
    (->> instr
         (partition-by #{[:inp :w]})
         (partition 2)
         (map-indexed (fn [i [_ ops]]
                (eval `(fnp ~(symbol (str "norm-" i)) [~arr in#]
                         (let [~w in#
                               ~x (aget ~arr 1)
                               ~y (aget ~arr 2)
                               ~z (aget ~arr 3)
                               ~ret (make-array Long/TYPE 4)
                               ~@(mapcat (fn [[i a1 a2]]
                                           `[~(to-sym a1) (~(op i) ~(to-sym a1) ~(if (keyword? a2) (to-sym a2) a2))])
                                         ops)]
                           (aset ~ret 0 ~(with-meta w {:tag "long"}))
                           (aset ~ret 1 ~(with-meta x {:tag "long"}))
                           (aset ~ret 2 ~(with-meta y {:tag "long"}))
                           (aset ~ret 3 ~(with-meta z {:tag "long"}))
                           ~ret))))))))

(defn run
  [input]
  (let [fns (to-fns input)
        counter (volatile! 0)
        h (fn rec [^longs start inputs fns]
            (vswap! counter inc)
            (when (zero? (rem @counter 100000000))
              (prn [:inputs inputs :start (seq start)]))
            (cond (and (empty? fns) (zero? (aget start 3)))
                  inputs
                  (empty? fns)
                  nil
                  :else
                  (or (rec ((first fns) start 9) (conj inputs 9) (rest fns))
                      (rec ((first fns) start 8) (conj inputs 8) (rest fns))
                      (rec ((first fns) start 7) (conj inputs 7) (rest fns))
                      (rec ((first fns) start 6) (conj inputs 6) (rest fns))
                      (rec ((first fns) start 5) (conj inputs 5) (rest fns))
                      (rec ((first fns) start 4) (conj inputs 4) (rest fns))
                      (rec ((first fns) start 3) (conj inputs 3) (rest fns))
                      (rec ((first fns) start 2) (conj inputs 2) (rest fns))
                      (rec ((first fns) start 1) (conj inputs 1) (rest fns)))))
        init (make-array Long/TYPE 4)]
    (h init [] fns)))

(def input (-> "data/day24"
               slurp
               clojure.string/split-lines
               parse))

(defn test-opt
  [input]
  (let [fns (to-fns input)
        opt-fns (to-fns-opt input)
        counter (volatile! 0)
        h (fn rec [^longs start ^longs s2 inputs fns opt-fns]
            (vswap! counter inc)
            (cond (> @counter 10000000)
                  true
                  (empty? fns)
                  (= (seq start) (seq s2))
                  :else
                  (and (rec ((first fns) start 9) ((first opt-fns) s2 9) (conj inputs 9) (rest fns) (rest opt-fns))
                       (rec ((first fns) start 8) ((first opt-fns) s2 8) (conj inputs 8) (rest fns) (rest opt-fns))
                       (rec ((first fns) start 7) ((first opt-fns) s2 7) (conj inputs 7) (rest fns) (rest opt-fns))
                       (rec ((first fns) start 6) ((first opt-fns) s2 6) (conj inputs 6) (rest fns) (rest opt-fns))
                       (rec ((first fns) start 5) ((first opt-fns) s2 5) (conj inputs 5) (rest fns) (rest opt-fns))
                       (rec ((first fns) start 4) ((first opt-fns) s2 4) (conj inputs 4) (rest fns) (rest opt-fns))
                       (rec ((first fns) start 3) ((first opt-fns) s2 3) (conj inputs 3) (rest fns) (rest opt-fns))
                       (rec ((first fns) start 2) ((first opt-fns) s2 2) (conj inputs 2) (rest fns) (rest opt-fns))
                       (rec ((first fns) start 1) ((first opt-fns) s2 1) (conj inputs 1) (rest fns) (rest opt-fns)))))
        init (make-array Long/TYPE 4)
        init2 (make-array Long/TYPE 4)]
    (h init init2 [] fns opt-fns)))

(defn part1
  [input]
  (tufte/add-basic-println-handler!
    {:format-pstats-opts {:columns [:total :clock :n-calls :p50 :mean]
                          :format-id-fn name}})
  (profile {} (test-opt input))
  #_(run input))

(defn part2
  [input])
