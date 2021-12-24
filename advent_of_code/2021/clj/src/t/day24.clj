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

(comment

(def monad (-> "data/day24"
             slurp
             (clojure.string/split-lines)
             parse
             to-fn
             eval))

(for [inp [
           [1 1 1 9 1 5 1 1 1 1 1 3 6 1]
           [1 1 1 9 1 5 1 1 1 1 1 3 6 2]
           [1 1 1 9 1 5 1 1 1 1 1 3 2 1]
           [1 1 1 9 1 5 1 1 1 1 1 2 6 1]
           [1 1 1 9 1 5 1 1 1 1 2 3 6 1]
           [1 1 1 9 1 5 1 1 1 2 1 3 6 1]
           [1 1 1 9 1 5 1 1 2 1 1 3 6 1]
           [1 1 1 9 1 5 1 2 1 1 1 3 6 1]
           [1 1 1 9 1 5 2 1 1 1 1 3 6 1]
           [1 1 1 9 1 2 1 1 1 1 1 3 6 1]
           [1 1 1 9 2 5 1 1 1 1 1 3 6 1]
           [1 1 1 2 1 5 1 1 1 1 1 3 6 1]
           [1 1 2 9 1 5 1 1 1 1 1 3 6 1]
           [1 2 1 9 1 5 1 1 1 1 1 3 6 1]
           [2 1 1 9 1 5 1 1 1 1 1 3 6 1]
           ]]
  [inp (monad inp)])


 [[1 7 1 9 1 1 1 1 1 1 1 1 1 1] [1 1 4 1782122]]
 [[1 7 1 9 1 1 1 1 1 1 1 3 1 1] [1 1 4 68540]]
 [[1 7 1 9 1 1 1 1 1 1 1 3 6 1] [1 0 0 2636]]
 [[1 7 1 9 1 5 1 1 1 1 1 3 6 1] [1 0 0 101]]
 [[1 7 1 9 1 5 1 1 1 1 1 3 6 1] [1 0 0 101]]
 [[1 7 1 9 1 5 1 2 1 1 1 3 6 1] [1 0 0 101]]
 [[1 7 2 9 1 5 1 1 1 1 1 3 6 1] [1 0 0 101]]
 [[1 2 1 9 1 5 1 1 1 1 1 3 6 1] [1 0 0 96]]
 [[2 7 1 9 1 5 1 1 1 1 1 3 6 1] [1 0 0 127]]


(for [inp [
           [9 8 4 9 1 9 5 9 9 9 7 9 9 4]
           [9 8 4 9 1 9 5 9 9 9 7 9 9 1]
           [9 8 4 9 1 9 5 9 9 9 7 9 9 2]
           [9 8 4 9 1 9 5 9 9 9 7 9 9 3]
           [9 8 4 9 1 9 5 9 9 9 7 9 9 5]
           [9 8 4 9 1 9 5 9 9 9 7 9 9 6]
           [9 8 4 9 1 9 5 9 9 9 7 9 9 7]
           [9 8 4 9 1 9 5 9 9 9 7 9 9 8]
           [9 8 4 9 1 9 5 9 9 9 7 9 9 9]
           ]]
  [inp (monad inp)])
 [[9 8 4 9 1 9 5 9 9 9 7 9 9 1] [1 1 4 4]]
 [[9 8 4 9 1 9 5 9 9 9 7 9 9 2] [2 1 5 5]]
 [[9 8 4 9 1 9 5 9 9 9 7 9 9 3] [3 1 6 6]]
 [[9 8 4 9 1 9 5 9 9 9 7 9 9 4] [4 0 0 0]]
 [[9 8 4 9 1 9 5 9 9 9 7 9 9 5] [5 1 8 8]]
 [[9 8 4 9 1 9 5 9 9 9 7 9 9 6] [6 1 9 9]]
 [[9 8 4 9 1 9 5 9 9 9 7 9 9 7] [7 1 10 10]]
 [[9 8 4 9 1 9 5 9 9 9 7 9 9 8] [8 1 11 11]]
 [[9 8 4 9 1 9 5 9 9 9 7 9 9 9] [9 1 12 12]]


(monad [7 3 4 9 1 7 3 9 9 9 6 8 4 2])
[2 0 0 0]
(monad [6 7 4 9 1 7 3 9 9 9 6 8 8 1])
[1 0 0 0]

(->> (range 0 100)
     (map (fn [i] (->> (format "%014d" i) (map #(Long/parseLong (str %))))))
     (map monad))


(->> [6 7 4 9 1 7 3 9 9 9 6 8 8 1]
     (apply str)
     Long/parseLong)
(/ (reduce - 0 [7 3 4 9 1 7 3 9 9 9 6 8 4 2]) 126.0)

(def i [1 1 9 9 9 6 2 6 4 4 2 4 9 4])

 (let [fitness (fn [i] (+ (/ (- 99999999999999 (Long/parseLong (apply str i)))
                             99999999999999.0)
                          (-> (monad i) (get 3))))
       mutate (fn [i] (assoc i (rand-int 14) (inc (rand-int 9))))
       crossover (fn [i1 i2]
                   (mapv (fn [x1 x2] (if (> 0.5 (rand)) x1 x2)) i1 i2))
       make-sol (fn [] (vec (repeatedly 14 #(inc (rand-int 9)))))
       carousel (fn [p] (let [maxi (reduce max (map first p))
                              inverted (map (fn [[f i]] [(- maxi f) f i]) p)
                              total (reduce + (map first inverted))
                              roll (rand total)]
                          (loop [r roll
                                 [[f' f s] & p] inverted]
                            (if (<= r f')
                              [f s]
                              (recur (- r f') p)))))]
   (defn genetic
     ([] (genetic (->> (repeatedly 100 make-sol)
                       (map (fn [i] [(fitness i) i]))
                       sort)))
     ([init-pop]
      (loop [population (sort init-pop)
             step 0]
        (if (== step 1000)
          population
          (recur (let [survivors (concat (take 10 population) (take 3 (reverse population)))
                       children (repeatedly 87 #(let [[_ parent1] (carousel population)
                                                      [_ parent2] (carousel population)
                                                      child (mutate (crossover parent1 parent2))]
                                                  [(fitness child) child]))]
                   (sort (concat survivors children)))
                 (inc step)))))))

(genetic)




                   )

(defn part1
  [input]
  (let [f (eval (to-fn input))]
    (loop [inputs (make-inputs input)
           n 0]
      (if (seq inputs)
        (let [in (first inputs)
              r (rest inputs)
              s (f in)]
          (when (zero? (mod n 1000000))
            (prn [in s]))
          (if (zero? (get s 3))
            (Long/parseLong (apply str in))
            (recur r (inc n))))
        ::error))))

(defn part2
  [input])
