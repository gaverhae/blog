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

(defn stack-exec-cont
  [ops]
  (let [compile-stack-op
        (fn [[op arg]]
          (case op
            :push (fn [^long ip stack]
                    [(inc ip) (conj stack arg)])
            :get (fn [^long ip stack]
                   [(inc ip) (conj stack (get stack arg))])
            :set (fn [^long ip stack]
                   (let [p (peek stack)
                         stack (pop stack)]
                     [(inc ip) (assoc stack arg p)]))
            :add (fn [^long ip stack]
                   (let [p1 (peek stack)
                         stack (pop stack)
                         p2 (peek stack)
                         stack (pop stack)]
                     [(inc ip) (conj stack (unchecked-add p2 p1))]))
            :not= (fn [^long ip stack]
                   (let [p1 (peek stack)
                         stack (pop stack)
                         p2 (peek stack)
                         stack (pop stack)]
                     [(inc ip) (conj stack (if (== p1 p2) 0 1))]))
            :jump-if-zero (fn [^long ip stack]
                            (let [p ^long (peek stack)
                                  stack (pop stack)
                                  nip (if (zero? p) arg (inc ip))]
                              [nip stack]))
            :jump (fn [^long _ stack]
                    [arg stack])
            :stop (fn [^long _ stack]
                    [nil stack])))
        tape (vec (map compile-stack-op (concat ops [[:stop]])))]
  (fn []
    (loop [[ip? stack] ((tape 0) 0 [])]
      (if ip?
        (recur ((tape ip?) ip? stack))
        stack)))))

(defn stack-exec-mut
  [ops]
  (let [compile-stack-op
        (fn [[op arg]]
          (case op
            :push (fn ^long [^long ip ^java.util.Stack stack]
                    (.push stack arg)
                    (unchecked-inc ip))
            :get (let [idx (int arg)]
                   (fn ^long [^long ip ^java.util.Stack stack]
                     (.push stack (.get stack idx))
                     (unchecked-inc ip)))
            :set (let [idx (int arg)]
                   (fn ^long [^long ip ^java.util.Stack stack]
                     (.set stack idx (.pop stack))
                     (unchecked-inc ip)))
            :add (fn ^long [^long ip ^java.util.Stack stack]
                   (.push stack (unchecked-add (.pop stack) (.pop stack)))
                   (unchecked-inc ip))
            :not= (fn ^long [^long ip ^java.util.Stack stack]
                    (.push stack (if (== (.pop stack) (.pop stack)) 0 1))
                    (unchecked-inc ip))
            :jump-if-zero (fn ^long [^long ip ^java.util.Stack stack]
                            (if (zero? (.pop stack))
                              (long arg)
                              (unchecked-inc ip)))
            :jump (let [idx (long arg)]
                    (fn ^long [^long _ ^java.util.Stack _]
                      arg))
            :stop (fn ^long [^long _ ^java.util.Stack _]
                    (long -1))))
        tape ^"[Ljava.lang.Object;" (into-array Object (map compile-stack-op (concat ops [[:stop]])))]
  (fn []
    (let [stack (java.util.Stack.)]
      (.push stack 0)
      (.push stack 0)
      (loop [ip (long 0)]
        (if (== (long -1) ip)
          (into [] stack)
          (recur (long ((aget tape ip) (long ip) stack)))))))))

(defn stack-exec-case
  [ops]
  (let [ip (gensym)
        stack (gensym)]
    (eval
      `(fn []
       (let [~stack (java.util.Stack.)]
         (.push ~stack 0)
         (.push ~stack 0)
         (loop [~ip (long 0)]
           ~(->> (concat ops [[:stop]])
                 (mapcat
                   (fn [idx [op arg]]
                     [idx (case op
                            :push `(do (.push ~stack ~arg)
                                       (recur ~(inc idx)))
                            :get `(do (.push ~stack (.get ~stack ~arg))
                                      (recur ~(inc idx)))
                            :set `(do (.set ~stack ~arg (.pop ~stack))
                                      (recur ~(inc idx)))
                            :add `(do (.push ~stack (unchecked-add (.pop ~stack) (.pop ~stack)))
                                      (recur ~(inc idx)))
                            :not= `(do (.push ~stack (if (== (.pop ~stack) (.pop ~stack)) 0 1))
                                       (recur ~(inc idx)))
                            :jump-if-zero `(do (if (zero? (.pop ~stack))
                                                 (recur ~arg)
                                                 (recur ~(inc idx))))
                            :jump `(recur ~arg)
                            :stop `(into [] ~stack))])
                   (range))
                 (concat ['case ip]))))))))

(defn stack-exec-case-jump
  [ops]
  (let [ops (concat ops [[:stop]])
        stack (gensym)
        ip (gensym)
        segments (->> ops
                      (filter (comp #{:jump :jump-if-zero} first))
                      (map second)
                      (cons 0)
                      set
                      (mapcat
                        (fn [ep]
                          [ep (->> (drop ep ops)
                                   (reduce (fn [acc [op :as code]]
                                             (if (= :jump op)
                                               (reduced (conj acc code))
                                               (conj acc code)))
                                           [])
                                   ((fn nest-jiz [[[op :as code] & tail ]]
                                      (cond
                                        (empty? tail) [code]
                                        (= :jump-if-zero op) [(conj code (nest-jiz tail))]
                                        :else (cons code (nest-jiz tail)))))
                                   (map (fn compile-op [[op arg tail]]
                                          (case op
                                            :push `(.push ~stack ~arg)
                                            :get `(.push ~stack (.get ~stack ~arg))
                                            :set `(.set ~stack ~arg (.pop ~stack))
                                            :add `(.push ~stack
                                                         (unchecked-add (.pop ~stack)
                                                                        (.pop ~stack)))
                                            :not= `(.push ~stack
                                                          (if (== (.pop ~stack)
                                                                  (.pop ~stack))
                                                            0
                                                            1))
                                            :jump-if-zero `(if (zero? (.pop ~stack))
                                                             (recur ~arg)
                                                             (do ~@(map compile-op tail)))
                                            :jump `(recur ~arg)
                                            :stop `(into [] ~stack))))
                                   (cons 'do))])))]
    (eval `(fn []
             (let [~stack (java.util.Stack.)]
               (.push ~stack 0)
               (.push ~stack 0)
               (loop [~ip (long 0)]
                 ~(concat ['case ip] segments)))))))

(comment

  (require '[criterium.core :as crit])

  (defmacro bench
    [exp]
    `(->> (crit/benchmark ~exp {}) :mean first (format "%1.2e")))

  (bench (baseline))
"2.66e-06"

  (bench (naive-ast-walk ast [nil nil]))
"5.33e-03"

  (def cc (compile-to-closure ast))
  (bench (cc [nil nil]))
"1.58e-03"

  (def sc (compile-stack ast))
  (= sc [[:push 100] [:set 0] [:push 1000] [:set 1] [:push 0] [:get 1] [:not=] [:jump-if-zero 27] [:get 0] [:push 4] [:add] [:get 0] [:add] [:push 3] [:add] [:set 0] [:get 0] [:push 2] [:add] [:push 4] [:add] [:set 0] [:push -1] [:get 1] [:add] [:set 1] [:jump 4] [:get 0]])
  (bench (run-stack sc))
"2.82e-02"

  (def scc (stack-exec-cont sc))
  (bench (scc))
"5.10e-03"

  (def scm (stack-exec-mut sc))
  (bench (scm))
"1.39e-03"

  (def sca (stack-exec-case sc))
  (bench (sca))
"7.01e-04"

  (def scj (stack-exec-case-jump sc))
  (bench (scj))
"5.92e-04"


  )
