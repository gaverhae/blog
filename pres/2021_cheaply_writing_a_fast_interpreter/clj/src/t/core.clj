(ns t.core
  (:require [datascript.core :as d]))

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
   [:return [:var 0]]])

(defn naive-ast-walk
  [expr]
  (let [h (fn h [expr env]
            (case (first expr)
              :do (reduce (fn [[_ env] expr] (h expr env))
                          [nil env]
                          (rest expr))
              :set (let [[_ idx e] expr
                         [v env] (h e env)]
                     [nil (assoc env idx v)])
              :lit [(second expr) env]
              :not= (let [[_ e1 e2] expr
                          [v1 env] (h e1 env)
                          [v2 env] (h e2 env)]
                      (if (== v1 v2)
                        [0 env]
                        [1 env]))
              :var (let [[_ idx] expr]
                     [(get env idx) env])
              :add (let [[_ e1 e2] expr
                         [v1 env] (h e1 env)
                         [v2 env] (h e2 env)]
                     [(unchecked-add v1 v2) env])
              :while (let [[_ e-condition e-body] expr]
                       (loop [env env]
                         (let [[condition env] (h e-condition env)]
                           (if (== condition 1)
                             (let [[_ env] (h e-body env)]
                               (recur env))
                             [nil env]))))
              :return (h (second expr) env)))]
    (first (h expr {}))))

(defn compile-to-closure
  [expr]
  (let [h (fn h [expr]
            (case (first expr)
              :do (let [do-body (reduce (fn [kont expr]
                                          (let [f (h expr)]
                                            (fn [env _]
                                              (let [[v env] (f env)]
                                                (kont env v)))))
                                        (fn [env v] [v env])
                                        (reverse (rest expr)))]
                    (fn [env] (do-body env nil)))
              :set (let [[_ idx e] expr
                         f (h e)]
                     (fn [env]
                       (let [[v env] (f env)]
                         [nil (assoc env idx v)])))
              :lit (let [e (second expr)]
                     (fn [env] [e env]))
              :not= (let [[_ e1 e2] expr
                          f1 (h e1)
                          f2 (h e2)]
                      (fn [env]
                        (let [[v1 env] (f1 env)
                              [v2 env] (f2 env)]
                          (if (== v1 v2)
                            [0 env]
                            [1 env]))))
              :var (let [[_ idx] expr]
                     (fn [env] [(get env idx) env]))
              :add (let [[_ e1 e2] expr
                         f1 (h e1)
                         f2 (h e2)]
                     (fn [env]
                       (let [[v1 env] (f1 env)
                             [v2 env] (f2 env)]
                         [(unchecked-add v1 v2) env])))
              :while (let [[_ e-condition e-body] expr
                           f-condition (h e-condition)
                           f-body (h e-body)]
                       (fn [env]
                         (let [[condition env] (f-condition env)]
                           (if (== condition 1)
                             (let [[_ env] (f-body env)]
                               (recur env))
                             [nil env]))))
              :return (h (second expr))))
        cc (h expr)]
    #(first (cc {}))))

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
                               [[:jump cur]]))
              :return (let [r (h cur (first args))]
                        (concat r [[:end]]))))]
    (vec (h 0 ast))))

(defn run-stack
  [code]
  ;; TODO: remove m structure
  (let [m {:code code
           :pointer 0
           :stack []}
        push (fn [m v] (update m :stack conj v))
        pop (fn [m] [(-> m :stack peek)
                     (update m :stack pop)])
        inc-pointer (fn [m] (update m :pointer inc))]
    (loop [m m]
      (let [[op arg] (get-in m [:code (:pointer m)])]
        (case op
          :push (recur (-> m
                           (push arg)
                           inc-pointer))
          :get (recur (-> m
                          (push (get-in m [:stack arg]))
                          inc-pointer))
          :not= (recur (let [[p1 m] (pop m)
                             [p2 m] (pop m)]
                         (-> m
                             (push (if (== p1 p2) 0 1))
                             inc-pointer)))
          :add (recur (let [[p1 m] (pop m)
                            [p2 m] (pop m)]
                        (-> m
                            (push (unchecked-add p1 p2))
                            inc-pointer)))
          :set (recur (let [[p m] (pop m)]
                        (-> m
                            (assoc-in [:stack arg] p)
                            inc-pointer)))
          :jump-if-zero (recur (let [[p m] (pop m)]
                                 (if (zero? p)
                                   (assoc m :pointer arg)
                                   (update m :pointer inc))))
          :jump (recur (assoc m :pointer arg))
          :end (-> m :stack peek))))))

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
            :end (fn [^long _ stack]
                    [nil stack])))
        tape (vec (map compile-stack-op ops))]
  (fn []
    (loop [[ip? stack] ((tape 0) 0 [])]
      (if ip?
        (recur ((tape ip?) ip? stack))
        (peek stack))))))

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
            :end (fn ^long [^long _ ^java.util.Stack _]
                    (long -1))))
        tape ^"[Ljava.lang.Object;" (into-array Object (map compile-stack-op ops))]
  (fn []
    (let [stack (java.util.Stack.)]
      (.push stack 0)
      (.push stack 0)
      (loop [ip (long 0)]
        (if (== (long -1) ip)
          (.pop stack)
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
           ~(->> ops
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
                            :end `(.pop ~stack))])
                   (range))
                 (concat ['case ip]))))))))

(defn stack-exec-case-jump
  [ops]
  (let [stack (gensym)
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
                                            :end `(.get ~stack 0))))
                                   (cons 'do))])))]
    (eval `(fn []
             (let [~stack (java.util.Stack.)]
               (.push ~stack 0)
               (.push ~stack 0)
               (loop [~ip (long 0)]
                 ~(concat ['case ip] segments)))))))

(defn compile-register-ssa
  ;;TODO: generate labels
  [ast]
  (let [max-var (fn max-var [[op & [arg1 arg2 :as args]]]
                  (case op
                    :lit 0
                    :return (max-var arg1)
                    :not= (max (max-var arg1)
                               (max-var arg2))
                    :add (max (max-var arg1)
                              (max-var arg2))
                    :var arg1
                    :set (max arg1 (max-var arg2))
                    :do (reduce max (map max-var args))
                    :while (max (max-var arg1)
                                (max-var arg2))))
        r (let [m (max-var ast)]
            (fn [i] (+ i m 1)))
        h (fn h [cur [op & [arg1 arg2 :as args]]]
            (case op
              :return (let [[r right] (h cur arg1)]
                        [nil (concat right
                                     [[:return r]])])
              :lit [(r cur) [[:load (r cur) arg1]]]
              :set (let [[r right] (h cur arg2)]
                     [nil (concat right
                                  [[:loadr arg1 r]])])
              :do [nil (->> args
                            (reduce (fn [[cur so-far] el]
                                      (let [[_ code] (h cur el)]
                                        [(+ cur (count code))
                                         (concat so-far code)]))
                                    [cur []])
                            second)]
              :while (let [[rcond condition] (h cur arg1)
                           [_ body] (h (+ cur 1 (count condition)) arg2)]
                       [nil (concat condition
                                    [[:jump-if-zero rcond (+ cur (count condition) 1 (count body) 1)]]
                                    body
                                    [[:jump cur]])])
              :var [(r cur) [[:loadr (r cur) arg1]]]
              :add (let [[rleft left] (h cur arg1)
                         [rright right] (h (+ cur (count left)) arg2)
                         rresult (+ cur (count left) (count right))]
                     [(r rresult) (concat
                                    left
                                    right
                                    [[:add (r rresult) rleft rright]])])
              :not= (let [[rleft left] (h cur arg1)
                          [rright right] (h (+ cur (count left)) arg2)
                          rresult (+ cur (count left) (count right))]
                      [(r rresult) (concat
                                     left
                                     right
                                     [[:not= (r rresult) rleft rright]])])))]
    (second (h 0 ast))))

(defn run-registers
  ;;TODO: handle labels
  [code]
  (let [tape (vec code)]
    (loop [i 0
           regs {}]
      (let [[ins arg1 arg2 arg3] (tape i)]
        (case ins
          :return (get regs arg1)
          :load (recur (inc i) (assoc regs arg1 arg2))
          :loadr (recur (inc i) (assoc regs arg1 (get regs arg2)))
          :jump-if-zero (if (zero? (get regs arg1))
                          (recur (long arg2) regs)
                          (recur (inc i) regs))
          :jump (recur (long arg1) regs)
          :add (recur (inc i)
                      (assoc regs arg1 (unchecked-add (get regs arg2)
                                                      (get regs arg3))))
          :not= (recur (inc i)
                       (assoc regs arg1 (if (== (get regs arg2) (get regs arg3))
                                          0 1))))))))

(defn optimize-register-code
  [code]
  (let [is-jump? {:jump 1, :jump-if-zero 2}
        make-labels (fn [code]
                      (let [jump-positions (->> code
                                                (filter (comp is-jump? first))
                                                (map (fn [op] (get op (is-jump? (first op)))))
                                                set)]
                        (loop [i 0
                               done []
                               todo code]
                          (if (seq todo)
                            (let [done (if-let [i (jump-positions i)]
                                         (conj done [:label i] (first todo))
                                         (conj done (first todo)))]
                              (recur (inc i) done (rest todo)))
                            done))))
        recalculate-jumps (fn [code]
                            (reduce (fn [code [_ label]]
                                      (let [idx (->> code
                                                     (keep-indexed #(when (= [:label label] %2) %1))
                                                     first)]
                                        (->> code
                                             (remove #{[:label label]})
                                             (map (fn [[op :as e]]
                                                    (if-let [j (is-jump? op)]
                                                      (update e j #(if (= label %)
                                                                     idx
                                                                     %))
                                                      e))))))
                                    code
                                    (->> code (filter (comp #{:label} first)))))
        patterns {'[[:add ?t ?a ?b] [:loadr ?r ?t]] ['[?r ?a ?b] #(->> % (cons :add) vec)]}
        ;; TODO: check for last read before write
        apply-pattern (fn [code [pattern [result f]]]
                        (let [len (count pattern)]
                          (loop [done []
                                 todo code]
                            (if (seq todo)
                              (if-let [match (d/q `[:find ~result
                                                    :where ~@pattern]
                                                  (take len todo))]
                                (recur done (cons (f match) (drop len todo)))
                                (recur (conj done (first todo)) (rest todo)))
                              done))))]
    (recalculate-jumps
      (reduce apply-pattern
              (make-labels code)
              patterns))))

(comment

  (require '[criterium.core :as crit])

  (defmacro bench
    [exp]
    `(->> (crit/benchmark ~exp {}) :mean first (format "%1.2e")))

  (bench (baseline))
"2.88e-06"

  (bench (naive-ast-walk ast))
"5.83e-03"

  (def cc (compile-to-closure ast))
  (bench (cc))
"2.00e-03"

  (def sc (compile-stack ast))
  (bench (run-stack sc))
"2.61e-02"

  (def scc (stack-exec-cont sc))
  (bench (scc))
"5.14e-03"

  (def scm (stack-exec-mut sc))
  (bench (scm))
"1.43e-03"

  (def sca (stack-exec-case sc))
  (bench (sca))
"6.94e-04"

  (def scj (stack-exec-case-jump sc))
  (bench (scj))
"6.01e-04"

  (def rc (compile-register-ssa ast))
  (bench (run-registers rc))
"7.63e-03"

  (def opt-rc (optimize-register-code rc))
  (bench (run-registers opt-rc))
"5.85e-03"

  )
