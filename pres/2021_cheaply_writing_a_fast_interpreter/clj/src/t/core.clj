(ns t.core)

(defn baseline
  []
  (loop [x (long 100)
         i (long 1000)]
    (if (zero? i)
      x
      (let [x (unchecked-add (unchecked-add (unchecked-add x 4) x) 3)
            x (unchecked-add (unchecked-add x 2) 4)]
        (recur x (unchecked-dec-int i))))))

(def ast
  [:do
   [:set 0 [:lit 100]]
   [:set 1 [:lit 1000]]
   [:while [:not= [:lit 0] [:var 1]]
    [:do
     [:set 0 [:add [:add [:add [:var 0] [:lit 4]] [:var 0]] [:lit 3]]]
     [:set 0 [:add [:add [:var 0] [:lit 2]] [:lit 4]]]
     [:set 1 [:add [:lit -1] [:var 1]]]]]
   [:var 0]])

(defn naive-ast-walk
  [expr env]
  (case (first expr)
    :do (reduce (fn [[_ env] expr] (naive-ast-walk expr env))
                [nil env]
                (rest expr))
    :set (let [[_ idx e] expr
               [v env] (naive-ast-walk e env)]
           [nil (assoc env idx v)])
    :lit [(second expr) env]
    :not= (let [[_ e1 e2] expr
                [v1 env] (naive-ast-walk e1 env)
                [v2 env] (naive-ast-walk e2 env)]
            (if (== v1 v2)
              [0 env]
              [1 env]))
    :var (let [[_ idx] expr]
           [(get env idx) env])
    :add (let [[_ e1 e2] expr
               [v1 env] (naive-ast-walk e1 env)
               [v2 env] (naive-ast-walk e2 env)]
           [(unchecked-add v1 v2) env])
    :while (let [[_ e-condition e-body] expr]
             (loop [env env]
               (let [[condition env] (naive-ast-walk e-condition env)]
                 (if (== condition 1)
                   (let [[_ env] (naive-ast-walk e-body env)]
                     (recur env))
                   [nil env]))))))

(defn compile-to-closure
  [expr]
  (case (first expr)
    :do (let [do-body (reduce (fn [kont expr]
                                (let [f (compile-to-closure expr)]
                                  (fn [env _]
                                    (let [[v env] (f env)]
                                      (kont env v)))))
                              (fn [env v] [v env])
                              (reverse (rest expr)))]
          (fn [env] (do-body env nil)))
    :set (let [[_ idx e] expr
               f (compile-to-closure e)]
           (fn [env]
             (let [[v env] (f env)]
               [nil (assoc env idx v)])))
    :lit (let [e (second expr)]
           (fn [env] [e env]))
    :not= (let [[_ e1 e2] expr
                f1 (compile-to-closure e1)
                f2 (compile-to-closure e2)]
            (fn [env]
              (let [[v1 env] (f1 env)
                    [v2 env] (f2 env)]
                (if (== v1 v2)
                  [0 env]
                  [1 env]))))
    :var (let [[_ idx] expr]
           (fn [env] [(get env idx) env]))
    :add (let [[_ e1 e2] expr
               f1 (compile-to-closure e1)
               f2 (compile-to-closure e2)]
           (fn [env]
             (let [[v1 env] (f1 env)
                   [v2 env] (f2 env)]
               [(unchecked-add v1 v2) env])))
    :while (let [[_ e-condition e-body] expr
                 f-condition (compile-to-closure e-condition)
                 f-body (compile-to-closure e-body)]
             (fn [env]
               (let [[condition env] (f-condition env)]
                 (if (== condition 1)
                   (let [[_ env] (f-body env)]
                     (recur env))
                   [nil env]))))))

(defn compile-stack
  [ast]
  (let [h (fn h [cur [op & args]]
            (case op
              :do (->> args
                       (reduce (fn [[cur so-far] el]
                            (let [code (h cur el)]
                              [(+ cur (count code))
                               (concat so-far code)]))
                          [0 []])
                       second)
              :lit [[:push (first args)]]
              :set (concat (h cur (second args))
                           [[:set (first args)]])
              :add (let [left (h cur (first args))
                         right (h (+ cur (count left)) (second args))]
                     (concat left right [[:add]]))
              :var [[:get (first args)]]
              :not= (let [left (h cur (first args))
                          right (h (+ cur (count left)) (second args))]
                      (concat left right [[:not=]]))
              :while (let [condition (h cur (first args))
                           body (h (+ cur 1 (count condition)) (second args))]
                       (concat condition
                               [[:jump-if-zero (+ cur
                                                  (count condition)
                                                  1
                                                  (count body)
                                                  1)]]
                               body
                               [[:jump cur]]))))]
    (vec (h 0 ast))))

(defn run-stack
  [code]
  (let [m {:code code
           :pointer 0
           :stack []}
        finished? (fn [m] (== (count (:code m))
                              (:pointer m)))
        push (fn [m v] (update m :stack conj v))
        pop (fn [m] [(-> m :stack peek)
                     (update m :stack pop)])
        inc-pointer (fn [m] (update m :pointer inc))]
    (loop [m m]
      (if (finished? m)
        [(peek (:stack m)) (:stack m)]
        (let [[op arg] (get-in m [:code (:pointer m)])]
          (recur (case op
                   :push (-> m
                             (push arg)
                             inc-pointer)
                   :get (-> m
                            (push (get-in m [:stack arg]))
                            inc-pointer)
                   :not= (let [[p1 m] (pop m)
                               [p2 m] (pop m)]
                           (-> m
                               (push (if (== p1 p2) 0 1))
                               inc-pointer))
                   :add (let [[p1 m] (pop m)
                              [p2 m] (pop m)]
                          (-> m
                              (push (unchecked-add p1 p2))
                              inc-pointer))
                   :set (let [[p m] (pop m)]
                          (-> m
                              (assoc-in [:stack arg] p)
                              inc-pointer))
                   :jump-if-zero
                   (let [[p m] (pop m)]
                     (if (zero? p)
                       (assoc m :pointer arg)
                       (update m :pointer inc)))
                   :jump (assoc m :pointer arg))))))))

(comment

  (require '[criterium.core :as crit])

  (defmacro bench
    [exp]
    `(->> (crit/benchmark ~exp {}) :mean first (format "%1.2e")))

  (bench (baseline))
"2.70e-06"

  (bench (naive-ast-walk ast [nil nil]))
"5.32e-03"

  (def cc (compile-to-closure ast))
  (bench (cc [nil nil]))
"1.59e-03"

  (def sc (compile-stack ast))
  (bench (run-stack sc))
"2.92e-02"

  )