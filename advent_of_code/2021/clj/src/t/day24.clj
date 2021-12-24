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

(defn fitness
  [i] (+ (/ (- 99999999999999 (Long/parseLong (apply str i)))
            99999999999999.0)
         (-> (monad i) (get 3))))

(def best-so-far
  [9 8 4 9 1 9 5 9 9 9 7 9 9 4])

 (let [mutate (fn [i] (update i (rand-int 14)
                              (fn [old]
                                (let [n (if (> (rand) 0.5)
                                          (inc old)
                                          (dec old))]
                                  (cond (== 0 n) 9
                                        (== 10 n) 1
                                        :else n)))))
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
        (if (== step 100)
          population
          (recur (let [survivors (concat (take 10 population) (take 3 (reverse population)))
                       make-child #(let [[_ parent1] (carousel population)
                                         [_ parent2] (carousel population)
                                         child (mutate (crossover parent1 parent2))]
                                     [(fitness child) child])]
                   (loop [nxt survivors
                          seen (set survivors)]
                     (if (== 100 (count nxt))
                       (sort nxt)
                       (let [child (make-child)]
                         (recur (if (or (seen child)
                                        (== -1 (compare (get child 1) best-so-far)))
                                  nxt (conj nxt child))
                                (conj seen child))))))
                 (inc step)))))))

(defn to
  [i]
  (vec (map #(Long/parseLong (str %)) (format "%014d" i))))

(defn from
  [v]
  (Long/parseLong (apply str v)))

(def primes (cons 2 (->> (iterate #(+ 2 %) 3)
                         (remove (fn [n]
                                   (->> primes
                                        (take-while #(<= (* % %) n))
                                        (some #(zero? (rem n %)))))))))
(defn prime?
  [n]
  (let [limit (Math/sqrt n)]
    (loop [primes primes]
      (let [p (first primes)]
        (cond (> p limit)
              true
              (zero? (rem n p))
              false
              :else
              (recur (rest primes)))))))

(defn div
  [n]
  (let [limit (Math/sqrt n)]
    (loop [primes primes]
      (let [p (first primes)]
        (cond (> p limit)
              nil
              (zero? (rem n p))
              p
              :else
              (recur (rest primes)))))))



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
