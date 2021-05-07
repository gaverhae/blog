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
   [:return [:var 0]]])

(defmacro match
  [expr & cases]
  (let [e (gensym)]
    `(let [~e ~expr]
       (case (first ~e)
         ~@(->> (partition 2 cases)
                (mapcat (fn [[pat body]]
                          [(first pat) `(let [~(vec (cons '_ (rest pat))) ~e]
                                          ~body)])))))))

(defn naive-ast-walk
  [expr]
  (let [h (fn h [expr env]
            (match expr
              [:do & sub-exprs] (reduce (fn [[_ env] expr] (h expr env))
                                        [nil env]
                                        sub-exprs)
              [:set idx e] (let [[v env] (h e env)]
                             [nil (assoc env idx v)])
              [:lit v] [v env]
              [:not= e1 e2] (let [[v1 env] (h e1 env)
                                  [v2 env] (h e2 env)]
                              (if (== v1 v2)
                                [0 env]
                                [1 env]))
              [:var idx] [(get env idx) env]
              [:add e1 e2] (let [[v1 env] (h e1 env)
                                 [v2 env] (h e2 env)]
                             [(unchecked-add v1 v2) env])
              [:while e-condition e-body] (loop [env env]
                                            (let [[condition env] (h e-condition env)]
                                              (if (== condition 1)
                                                (let [[_ env] (h e-body env)]
                                                  (recur env))
                                                [nil env])))
              [:return e] (h e env)))]
    (first (h expr {}))))

(defn compile-to-closure
  [expr]
  (let [h (fn h [expr]
            (match expr
              [:do & sub-exprs] (let [do-body (reduce (fn [kont expr]
                                                        (let [f (h expr)]
                                                          (fn [env _]
                                                            (let [[v env] (f env)]
                                                              (kont env v)))))
                                                      (fn [env v] [v env])
                                                      (reverse sub-exprs))]
                                  (fn [env] (do-body env nil)))
              [:set idx e] (let [f (h e)]
                             (fn [env]
                               (let [[v env] (f env)]
                                 [nil (assoc env idx v)])))
              [:lit e] (fn [env] [e env])
              [:not=  e1 e2] (let [f1 (h e1)
                                   f2 (h e2)]
                               (fn [env]
                                 (let [[v1 env] (f1 env)
                                       [v2 env] (f2 env)]
                                   (if (== v1 v2)
                                     [0 env]
                                     [1 env]))))
              [:var idx] (fn [env] [(get env idx) env])
              [:add e1 e2] (let [f1 (h e1)
                                 f2 (h e2)]
                             (fn [env]
                               (let [[v1 env] (f1 env)
                                     [v2 env] (f2 env)]
                                 [(unchecked-add v1 v2) env])))
              [:while e-condition e-body] (let [f-condition (h e-condition)
                                                f-body (h e-body)]
                                            (fn [env]
                                              (let [[condition env] (f-condition env)]
                                                (if (== condition 1)
                                                  (let [[_ env] (f-body env)]
                                                    (recur env))
                                                  [nil env]))))
              [:return e] (h e)))
        cc (h expr)]
    #(first (cc {}))))

(defn compile-stack
  [ast]
  (let [h (fn h [cur expr]
            (match expr
              [:do & sub-exprs] (->> sub-exprs
                                     (reduce (fn [[cur so-far] el]
                                               (let [code (h cur el)]
                                                 [(+ cur (count code))
                                                  (concat so-far code)]))
                                             [0 []])
                                     second)
              [:lit v] [[:push v]]
              [:set idx e] (concat (h cur e)
                                   [[:set idx]])
              [:add e1 e2] (let [left (h cur e1)
                                 right (h (+ cur (count left)) e2)]
                             (concat left right [[:add]]))
              [:var idx] [[:get idx]]
              [:not= e1 e2] (let [left (h cur e1)
                                  right (h (+ cur (count left)) e2)]
                              (concat left right [[:not=]]))
              [:while cnd bod] (let [condition (h cur cnd)
                                     body (h (+ cur 1 (count condition)) bod)]
                                 (concat condition
                                         [[:jump-if-zero (+ cur
                                                            (count condition)
                                                            1
                                                            (count body)
                                                            1)]]
                                         body
                                         [[:jump cur]]))
              [:return e] (let [r (h cur e)]
                            (concat r [[:end]]))))]
    (vec (h 0 ast))))

(defn run-stack
  [code]
  (loop [pc 0
         stack []]
    (let [op (code pc)]
      (match op
        [:push val] (recur (inc pc) (conj stack val))
        [:get idx] (recur (inc pc) (conj stack (stack idx)))
        [:not=] (let [p1 (peek stack)
                      stack (pop stack)
                      p2 (peek stack)
                      stack (pop stack)]
                  (recur (inc pc) (conj stack (if (== p1 p2) 0 1))))
        [:add] (let [p1 (peek stack)
                     stack (pop stack)
                     p2 (peek stack)
                     stack (pop stack)]
                 (recur (inc pc) (conj stack (unchecked-add p1 p2))))
        [:set idx] (let [p (peek stack)
                         stack (pop stack)]
                     (recur (inc pc) (assoc stack idx p)))
        [:jump-if-zero to] (let [p (peek stack)
                                 stack (pop stack)]
                             (if (zero? p)
                               (recur (long to) stack)
                               (recur (inc pc) stack)))
        [:jump to] (recur (long to) stack)
        [:end] (peek stack)))))

(defn stack-exec-cont
  [ops]
  (let [compile-stack-op
        (fn [op]
          (match op
            [:push val] (fn [^long ip stack]
                          [(inc ip) (conj stack val)])
            [:get idx] (fn [^long ip stack]
                         [(inc ip) (conj stack (get stack idx))])
            [:set idx] (fn [^long ip stack]
                         (let [p (peek stack)
                               stack (pop stack)]
                           [(inc ip) (assoc stack idx p)]))
            [:add] (fn [^long ip stack]
                     (let [p1 (peek stack)
                           stack (pop stack)
                           p2 (peek stack)
                           stack (pop stack)]
                       [(inc ip) (conj stack (unchecked-add p2 p1))]))
            [:not=] (fn [^long ip stack]
                      (let [p1 (peek stack)
                            stack (pop stack)
                            p2 (peek stack)
                            stack (pop stack)]
                        [(inc ip) (conj stack (if (== p1 p2) 0 1))]))
            [:jump-if-zero to] (fn [^long ip stack]
                                 (let [p ^long (peek stack)
                                       stack (pop stack)
                                       nip (if (zero? p) to (inc ip))]
                                   [nip stack]))
            [:jump to] (fn [^long _ stack]
                         [to stack])
            [:end] (fn [^long _ stack]
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
        (fn [op]
          (match op
            [:push v] (fn ^long [^long ip ^java.util.Stack stack]
                        (.push stack v)
                        (unchecked-inc ip))
            [:get idx] (let [idx (int idx)]
                         (fn ^long [^long ip ^java.util.Stack stack]
                           (.push stack (.get stack idx))
                           (unchecked-inc ip)))
            [:set idx] (let [idx (int idx)]
                         (fn ^long [^long ip ^java.util.Stack stack]
                           (.set stack idx (.pop stack))
                           (unchecked-inc ip)))
            [:add] (fn ^long [^long ip ^java.util.Stack stack]
                     (.push stack (unchecked-add (.pop stack) (.pop stack)))
                     (unchecked-inc ip))
            [:not=] (fn ^long [^long ip ^java.util.Stack stack]
                      (.push stack (if (== (.pop stack) (.pop stack)) 0 1))
                      (unchecked-inc ip))
            [:jump-if-zero to] (fn ^long [^long ip ^java.util.Stack stack]
                                 (if (zero? (.pop stack))
                                   (long to)
                                   (unchecked-inc ip)))
            [:jump to] (let [idx (long to)]
                         (fn ^long [^long _ ^java.util.Stack _]
                           to))
            [:end] (fn ^long [^long _ ^java.util.Stack _]
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
                   (fn [idx op]
                     [idx (match op
                            [:push v] `(do (.push ~stack ~v)
                                           (recur ~(inc idx)))
                            [:get i] `(do (.push ~stack (.get ~stack ~i))
                                            (recur ~(inc idx)))
                            [:set i] `(do (.set ~stack ~i (.pop ~stack))
                                            (recur ~(inc idx)))
                            [:add] `(do (.push ~stack (unchecked-add (.pop ~stack) (.pop ~stack)))
                                        (recur ~(inc idx)))
                            [:not=] `(do (.push ~stack (if (== (.pop ~stack) (.pop ~stack)) 0 1))
                                         (recur ~(inc idx)))
                            [:jump-if-zero to] `(do (if (zero? (.pop ~stack))
                                                      (recur ~to)
                                                      (recur ~(inc idx))))
                            [:jump to] `(recur ~to)
                            [:end] `(.pop ~stack))])
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
                                   (map (fn compile-op [op]
                                          (match op
                                            [:push v] `(.push ~stack ~v)
                                            [:get i] `(.push ~stack (.get ~stack ~i))
                                            [:set i] `(.set ~stack ~i (.pop ~stack))
                                            [:add] `(.push ~stack
                                                           (unchecked-add (.pop ~stack)
                                                                          (.pop ~stack)))
                                            [:not=] `(.push ~stack
                                                            (if (== (.pop ~stack)
                                                                    (.pop ~stack))
                                                              0
                                                              1))
                                            [:jump-if-zero to tail] `(if (zero? (.pop ~stack))
                                                                  (recur ~to)
                                                                  (do ~@(map compile-op tail)))
                                            [:jump to] `(recur ~to)
                                            [:end] `(.get ~stack 0))))
                                   (cons 'do))])))]
    (eval `(fn []
             (let [~stack (java.util.Stack.)]
               (.push ~stack 0)
               (.push ~stack 0)
               (loop [~ip (long 0)]
                 ~(concat ['case ip] segments)))))))

(defmacro mdo
  [bindings]
  (if (#{0 1} (count bindings))
    (throw (RuntimeException. "invalid number of elements in mdo bindings"))
    (let [[n v & r] bindings]
      (if (empty? r)
        v
        [:bind v `(fn [~n] (mdo ~r))]))))

(defn compile-register-ssa
  [ast]
  (let [max-var ((fn max-var [op]
                   (match op
                     [:lit _] 0
                     [:return e] (max-var e)
                     [:not= e1 e2] (max (max-var e1)
                                        (max-var e2))
                     [:add e1 e2] (max (max-var e1)
                                       (max-var e2))
                     [:var i] i
                     [:set i e] (max i (max-var e))
                     [:do & es] (reduce max (map max-var es))
                     [:while c b] (max (max-var c)
                                       (max-var b)))) ast)
        run (fn run [s ma]
              (match ma
                [:pure a] [s a]
                [:bind ma f] (let [[s a] (run s ma)]
                               (run s (f a)))
                [:current-position] [s (-> s :code count dec)]
                [:free-register] [(update s :reg inc) (:reg s)]
                [:emit code] [(update s :code conj code) nil]
                [:hoist idx v] [(update s :hoisted assoc idx v) nil]
                [:future] [(-> s
                               (update :code conj :placeholder)
                               (update :nested conj (-> s :code count)))
                           nil]
                [:resolve f] [(-> s
                                  (update :code assoc (-> s :nested peek) (f (-> s :code count)))
                                  (update :nested pop))
                              nil]))
        h (fn h [op & [ret]]
            (match op
              [:return e] (mdo [r (h e)
                                _ [:emit [:return r]]])
              [:lit v] (if ret
                         [:emit [:load ret v]]
                         (mdo [r [:free-register]
                               _ [:hoist r v]
                               _ [:pure r]]))
              [:set idx e] (if (#{:lit :add :not=} (first e))
                             (h e idx)
                             (mdo [r (h e)
                                   _ [:emit [:loadr idx r]]]))
              [:do & sub] (if (empty? sub)
                            [:pure nil]
                            (mdo [_ (h (first sub))
                                  _ (h (cons :do (rest sub)))]))
              [:while cnd bod] (mdo [before-condition [:current-position]
                                     condition (h cnd)
                                     _ [:future]
                                     body (h bod)
                                     _ [:emit [:jump (inc before-condition)]]
                                     _ [:resolve (fn [pos] [:jump-if-zero condition pos])]])
              [:var idx] [:pure idx]
              [:add e1 e2] (mdo [left (h e1)
                                 right (h e2)
                                 r (if ret [:pure ret] [:free-register])
                                 _ [:emit [:add r left right]]
                                 _ [:pure r]])
              [:not= e1 e2] (mdo [left (h e1)
                                  right (h e2)
                                  r (if ret [:pure ret] [:free-register])
                                  _ [:emit [:not= r left right]]
                                  _ [:pure r]])))]
    (-> (run {:nested (), :code [], :hoisted {}, :reg (inc max-var)}
             (h ast))
        first
        (dissoc :nested :reg))))

(defmacro match-arr
  [expr & cases]
  (let [e (with-meta (gensym)
                     (meta expr))]
    `(let [~e ~expr]
       (case (aget ~e 0)
         ~@(->> (partition 2 cases)
                (mapcat (fn [[pat body]]
                          [(first pat) `(let ~(->> pat
                                                   (map-indexed vector)
                                                   rest
                                                   (mapcat (fn [[idx v]] `[~v (aget ~e ~idx)]))
                                                   vec)
                                          ~body)])))))))

(defn run-registers
  [{:keys [code hoisted]}]
  (let [max-reg (max (->> hoisted keys (reduce max))
                     (->> code
                          (remove (comp #{:jump} first))
                          (map second)
                          (reduce max)))
        registers (long-array (inc max-reg))
        ^"[[J" tape (->> code
                         (map (fn [op]
                                (let [r (long-array (count op))]
                                  (aset r 0 (case (first op)
                                              :return 0
                                              :load 1
                                              :loadr 2
                                              :jump-if-zero 3
                                              :jump 4
                                              :add 5
                                              :not= 6))
                                  (doseq [n (range 1 (count op))]
                                    (aset r n (long (get op n))))
                                  r)))
                         into-array)]
    (doseq [[k v] hoisted]
      (aset ^longs registers (int k) (long v)))
    (loop [i 0]
      (match-arr ^"[J" (aget tape i)
        [0 r] (aget registers (int r))
        [1 r v] (do (aset registers (int r) (long v))
                        (recur (inc i)))
        [2 into from] (do (aset registers (int into) (aget registers (int from)))
                               (recur (inc i)))
        [3 r to] (if (zero? (aget registers (int r)))
                               (recur (long to))
                               (recur (inc i)))
        [4 to] (recur (long to))
        [5 result op1 op2] (do (aset registers (int result) (unchecked-add (aget registers (int op1))
                                                                              (aget registers (int op2))))
                                  (recur (inc i)))
        [6 result op1 op2] (do (aset registers (int result) (if (== (aget registers (int op1))
                                                                        (aget registers (int op2)))
                                                                  0 1))
                                   (recur (inc i)))))))

(comment

  [:bind ma f]
  [:return a]

  (defn run
    [ma]
    (match ma
      [:return v] v
      [:pure v] v
      [:bind ma f] (run (f (run ma)))))

  (run (mdo [a [:pure 15]
             b [:pure 18]
             _ [:return (+ a b)]]))
33

(defn mdo'
    [bindings]
    (if (#{0 1} (count bindings))
      (throw (RuntimeException. "invalid number of elements in mdo bindings"))
      (let [[n v & r] bindings]
        (if (empty? r)
          v
          [:bind v `(fn [~n] (mdo ~r))]))))
(mdo' ['a [:pure 15]
             'b [:pure 18]
             '_ [:return '(+ a b)]])
[:bind [:pure 15] (fn [a] [:bind [:pure 18] (fn [b] [:return (+ a b)])])]

  (defn run-output
    [ma]
    (match ma
      [:return v] [[] v]
      [:pure v] [[] v]
      [:bind ma f] (let [[prev a] (run-output ma)
                         [next b] (run-output (f a))]
                     [(concat prev next) b])
      [:output l] [[l] nil]))

  (run-output (mdo [a [:pure 1]
                    _ [:output a]
                    b [:pure (+ a 3)]
                    _ [:output b]
                    c [:pure (+ b 4)]
                    _ [:output c]
                    result [:pure (* 3 c)]
                    _ [:output result]
                    _ [:return result]]))
[(1 4 8 24) 24]

  (defn run-stack
    [stack ma]
    (match ma
      [:return v] [stack v]
      [:pure v] [stack v]
      [:bind ma f] (let [[new-stack a] (run-stack stack ma)]
                     (run-stack new-stack (f a)))
      [:pop] [(pop stack) (peek stack)]
      [:push e] [(conj stack e) nil]))

  (run-stack [10 12] (mdo [a [:pop]
                           b [:pop]
                           _ [:push (+ a b)]]))
[[22] nil]

             )

(comment

  (require '[criterium.core :as crit])

  (defmacro bench
    [exp]
    `(->> (crit/benchmark ~exp {}) :mean first (format "%1.2e")))

  (bench (baseline))
"2.68e-06"

  (bench (naive-ast-walk ast))
"5.30e-03"

  (def cc (compile-to-closure ast))
  (bench (cc))
"1.94e-03"

  (def sc (compile-stack ast))
  (bench (run-stack sc))
"6.51e-03"

  (def scc (stack-exec-cont sc))
  (bench (scc))
"5.59e-03"

  (def scm (stack-exec-mut sc))
  (bench (scm))
"1.40e-03"

  (def sca (stack-exec-case sc))
  (bench (sca))
"6.64e-04"

  (def scj (stack-exec-case-jump sc))
  (bench (scj))
"5.70e-04"

  (def rc (compile-register-ssa ast))
  (bench (run-registers rc))
"1.82e-04"

  )
