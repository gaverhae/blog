(ns t.day24)

(defn parse
  [lines]
  (->> lines
       (map (fn [l] (let [[_ op arg1 _ arg2] (re-matches #"(...) (.)( (.))?" l)]
                      [(keyword op) (keyword arg1) arg2])))
       (map (fn [[op arg1 arg2]]
              (if (= op :inp)
                [:inp arg1]
                [op arg1 (if (Character/isDigit ^Character (first arg2))
                           (Long/parseLong arg2)
                           (keyword arg2))])))))

(defn to-fn
  [instr]
  (let [input (gensym "input")
        w (gensym "w")
        x (gensym "x")
        y (gensym "y")
        z (gensym "z")]
    `(fn [~input]
       (let [~w 0 ~x 0 ~y 0 ~z 0
             ~@(->> instr
                    (mapcat (fn [[i a1 a2]]
                              (let [to-sym {:w w, :x x, :y y, :z z}
                                    op {:add +, :mul *, :div quot, :mod mod, :eql (fn [a b] (if (== a b) 1 0))}]
                                (if (= i :inp)
                                  `[~(to-sym a1) (first ~input) ~input (rest ~input)]
                                  `[~(to-sym a1) (~(op i) ~(to-sym a1) ~(if (keyword? a2) (to-sym a2) a2))])))))]
         [~w ~x ~y ~z]))))

(defn make-inputs
  [input]
  (let [size (->> input (map first) (filter #{:inp}) count)
        vs [9 8 7 6 5 4 3 2 1]
        help (fn help [i acc]
               (if (zero? i)
                 [acc]
                 (for [ls (help (dec i) acc)
                       v vs]
                   (conj ls v))))]
    (help size [])))

(defn part1
  [input]
  (let [f (eval (to-fn input))]
    (->> (make-inputs input)
         (map (fn [input] (cons input (f input))))
         (filter (fn [[in w x y z]] (zero? z)))
         ffirst
         (apply str)
         Long/parseLong)))

(defn part2
  [input])
