(ns t.day24)

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
         (map (fn [[_ ops]]
                (eval `(fn [~arr in#]
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
            (when (zero? (rem @counter 10000000))
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


(comment

  (def input (-> "data/day24"
                 slurp
                 clojure.string/split-lines
                 parse))

  (to-fns input)

  (run input)

  )

(defn part1
  [input]
  )

(defn part2
  [input])
