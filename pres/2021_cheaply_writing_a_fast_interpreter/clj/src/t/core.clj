(ns t.core
  (:require [clojure.java.shell :as shell]
            [clojure.string :as string]))

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
   [:do
    [:set 1 [:lit 1000]]
    [:do
     [:while
      [:bin :not= [:lit 0] [:var 1]]
      [:do
       [:set 0 [:bin :add [:bin :add [:bin :add [:var 0] [:lit 4]]
                                     [:var 0]]
                          [:lit 3]]]
       [:do
        [:set 0 [:bin :add [:bin :add [:var 0] [:lit 2]]
                           [:lit 4]]]
        [:set 1 [:bin :add [:lit -1] [:var 1]]]]]]
     [:var 0]]]])

(def bin
  {:add (fn [^long a ^long b] (unchecked-add a b))
   :not= (fn [^long a ^long b] (if (== a b) 0 1))})

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
              [:lit v] [v env]
              [:var idx] [(get env idx) env]
              [:set idx e] (let [[v env] (h e env)]
                             [v (assoc env idx v)])
              [:bin op e1 e2] (let [f (bin op)
                                    [v1 env] (h e1 env)
                                    [v2 env] (h e2 env)]
                                [(f v1 v2) env])
              [:do head tail] (let [[v env] (h head env)]
                                (h tail env))
              [:while e-condition e-body] (loop [env env]
                                            (let [[condition env] (h e-condition env)]
                                              (if (== condition 1)
                                                (let [[_ env] (h e-body env)]
                                                  (recur env))
                                                [nil env])))))]
    (first (h expr {}))))

(defmacro mdo
  [bindings]
  (if (#{0 1} (count bindings))
    (throw (RuntimeException. "invalid number of elements in mdo bindings"))
    (let [[n v & r] bindings]
      (if (empty? r)
        v
        [:bind v `(fn [~n] (mdo ~r))]))))

(defn twe-mon
  [expr]
  (let [eval (fn eval [exp]
               (match exp
                 [:lit v] [:return v]
                 [:var n] (mdo [v [:lookup n]
                                _ [:return v]])
                 [:set n e] (mdo [v (eval e)
                                  _ [:set n v]
                                  _ [:return v]])
                 [:bin op e1 e2] (mdo [v1 (eval e1)
                                       v2 (eval e2)
                                       _ [:return ((bin op) v1 v2)]])
                 [:do head tail] (mdo [_ (eval head)
                                       _ (eval tail)])
                 [:while condition body] (mdo [c (eval condition)
                                               _ (if (== 1 c)
                                                   (mdo [_ (eval body)
                                                         _ (eval exp)])
                                                   [:return nil])])))
        exec (fn exec [m env cont]
               #(match m
                  [:bind ma f] (exec ma env (fn [env ret] (exec (f ret) env cont)))
                  [:return a] (cont env a)
                  [:lookup n] (cont env (env n))
                  [:set n v] (cont (assoc env n v) nil)))]
    (trampoline (exec (eval expr) {} (fn [_ v] v)))))

(defn compile-to-closure
  [expr]
  (let [h (fn h [expr]
            (match expr
              [:lit e] (fn [env] [e env])
              [:var idx] (fn [env] [(get env idx) env])
              [:set idx e] (let [f (h e)]
                             (fn [env]
                               (let [[v env] (f env)]
                                 [v (assoc env idx v)])))
              [:bin op e1 e2] (let [f (bin op)
                                    f1 (h e1)
                                    f2 (h e2)]
                                (fn [env]
                                  (let [[v1 env] (f1 env)
                                        [v2 env] (f2 env)]
                                    [(f v1 v2) env])))
              [:do head tail] (let [head-body (h head)
                                    tail-body (h tail)]
                                (fn [env]
                                  (let [[v env] (head-body env)]
                                    (tail-body env))))
              [:while e-condition e-body] (let [f-condition (h e-condition)
                                                f-body (h e-body)]
                                            (fn [env]
                                              (let [[condition env] (f-condition env)]
                                                (if (== condition 1)
                                                  (let [[_ env] (f-body env)]
                                                    (recur env))
                                                  [nil env]))))))
        cc (h expr)]
    #(first (cc {}))))

(defn twe-cont
  [expr]
  (let [h (fn h [expr env cont]
            #(match expr
               [:lit v] (cont env v)
               [:var idx] (cont env (get env idx))
               [:set idx e] (h e env (fn [env v] (cont (assoc env idx v) v)))
               [:bin op e1 e2] (h e1 env
                                  (fn [env v1]
                                    (h e2 env
                                       (fn [env v2]
                                         (cont env ((bin op) v1 v2))))))
               [:do head tail] (h head env (fn [env _] (h tail env cont)))
               [:while e-condition e-body]
               (h e-condition env
                 (fn [env c]
                   (if (== 1 c)
                     (h e-body env (fn [env _] (h expr env cont)))
                     (cont env nil))))))]
    (h expr {} (fn [_ v] v))))

(defn compile-stack
  [ast]
  (let [h (fn h [cur expr]
            (match expr
              [:lit v] [[:push v]]
              [:var idx] [[:get idx]]
              [:set idx e] (concat (h cur e)
                                   [[:set idx]])
              [:bin op e1 e2] (let [left (h cur e1)
                                    right (h (+ cur (count left)) e2)]
                                (concat left right [[:bin op]]))
              [:do head tail] (let [hd (h cur head)
                                    tl (h (+ cur (count hd)) tail)]
                                (concat hd tl))
              [:while cnd bod] (let [condition (h cur cnd)
                                     body (h (+ cur 1 (count condition)) bod)]
                                 (concat condition
                                         [[:jump-if-zero (+ cur
                                                            (count condition)
                                                            1
                                                            (count body)
                                                            1)]]
                                         body
                                         [[:jump cur]]))))]
    (-> (h 0 ast)
        vec
        (conj [:end]))))

(defn run-stack
  [code]
  (loop [pc 0
         stack []]
    (let [op (code pc)]
      (match op
        [:push val] (recur (inc pc) (conj stack val))
        [:set idx] (let [p (peek stack)
                         stack (pop stack)]
                     (recur (inc pc) (assoc stack idx p)))
        [:get idx] (recur (inc pc) (conj stack (stack idx)))
        [:bin op] (let [f (bin op)
                        p1 (peek stack)
                        stack (pop stack)
                        p2 (peek stack)
                        stack (pop stack)]
                    (recur (inc pc) (conj stack (f p1 p2))))
        [:jump to] (recur (long to) stack)
        [:jump-if-zero to] (let [p (peek stack)
                                 stack (pop stack)]
                             (if (zero? p)
                               (recur (long to) stack)
                               (recur (inc pc) stack)))
        [:end] (peek stack)))))

(defn run-stack-mut
  [code]
  (let [^longs stack (long-array 256)
        bin-map (->> bin (map-indexed (fn [idx [kw _]] [kw idx])) (into {}))
        ^objects bin (->> bin (map second) (into-array Object))
        ^ints code (->> code
                        (mapcat #(match %
                                   [:push val] [0 val]
                                   [:set idx] [1 idx]
                                   [:get idx] [2 idx]
                                   [:bin op] [3 (bin-map op)]
                                   [:jump to] [4 (* 2 to)]
                                   [:jump-if-zero to] [5 (* 2 to)]
                                   [:end] [6 -1]))
                        (into-array Integer/TYPE))]
    #(loop [pc (int 0)
            top (int 0)]
       (case (aget code pc)
         0 (do (aset stack top (aget code (unchecked-inc-int pc)))
               (recur (unchecked-add-int pc 2)
                      (unchecked-inc-int top)))
         1 (do (aset stack (aget code (unchecked-inc-int pc)) (aget stack (unchecked-dec-int top)))
               (recur (unchecked-add-int pc 2)
                      (Math/max (unchecked-inc-int (aget code (unchecked-inc-int pc)))
                                (unchecked-dec-int top))))
         2 (do (aset stack top (aget stack (aget code (unchecked-inc-int pc))))
               (recur (unchecked-add-int pc 2)
                      (unchecked-inc-int top)))
         3 (do (let [f ^IFn (aget bin (aget code (unchecked-inc-int pc)))]
                 (aset stack
                       (unchecked-subtract-int top 2)
                       (long (f (aget stack (unchecked-dec-int top))
                                (aget stack (unchecked-subtract-int top 2)))))
                 (recur (unchecked-add-int pc 2)
                        (unchecked-dec-int top))))
         4 (recur (aget code (unchecked-inc-int pc))
                  top)
         5 (if (zero? (aget stack top))
             (recur (aget code (unchecked-inc-int pc))
                    (unchecked-dec-int top))
             (recur (unchecked-add-int pc 2)
                    (unchecked-dec-int top)))
         6 (aget stack (unchecked-dec-int top))))))

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
            [:bin op] (fn [^long ip stack]
                        (let [f (bin op)
                              p1 (peek stack)
                              stack (pop stack)
                              p2 (peek stack)
                              stack (pop stack)]
                          [(inc ip) (conj stack (f p2 p1))]))
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
            [:bin op] (let [f (bin op)]
                        (fn ^long [^long ip ^java.util.Stack stack]
                          (.push stack (f (.pop stack) (.pop stack)))
                          (unchecked-inc ip)))
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
                            [:bin op] (let [f (bin op)]
                                        `(do (.push ~stack (~f (.pop ~stack) (.pop ~stack)))
                                             (recur ~(inc idx))))
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
                                            [:bin op] (let [f (bin op)]
                                                        `(.push ~stack
                                                                (~f (.pop ~stack)
                                                                    (.pop ~stack))))
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

(defn compile-register-ssa
  [ast]
  (let [max-var ((fn max-var [op]
                   (match op
                     [:lit _] 0
                     [:bin op e1 e2] (max (max-var e1)
                                          (max-var e2))
                     [:var i] i
                     [:set i e] (max i (max-var e))
                     [:do head tail] (max (max-var head)
                                          (max-var tail))
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
              [:lit v] (if ret
                         [:emit [:load ret v]]
                         (mdo [r [:free-register]
                               _ [:hoist r v]
                               _ [:pure r]]))
              [:set idx e] (if (#{:lit :bin} (first e))
                             (h e idx)
                             (mdo [r (h e)
                                   _ [:emit [:loadr idx r]]]))
              [:do head tail] (mdo [_ (h head)
                                    _ (h tail)])
              [:while cnd bod] (mdo [before-condition [:current-position]
                                     condition (h cnd)
                                     _ [:future]
                                     body (h bod)
                                     _ [:emit [:jump (inc before-condition)]]
                                     _ [:resolve (fn [pos] [:jump-if-zero condition pos])]])
              [:var idx] [:pure idx]
              [:bin op e1 e2] (mdo [left (h e1)
                                    right (h e2)
                                    r (if ret [:pure ret] [:free-register])
                                    _ [:emit [[:bin op] r left right]]
                                    _ [:pure r]])))]
    (let [[s a] (run {:nested (), :code [], :hoisted {}, :reg (inc max-var)}
                     (h ast))]
      (-> s
          (update :code conj [:return a])
          (dissoc :nested :reg)))))

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
                                              [:bin :add] 5
                                              [:bin :not=] 6))
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

(defn registers-jump
  [{:keys [code hoisted]}]
  (let [registers (gensym)
        ip (gensym)
        is-jump? (comp #{:jump :jump-if-zero} first)
        re-get (fn [r] `(long ~(hoisted r `(aget ~registers (int ~r)))))
        segments (->> code
                      (map-indexed vector)
                      (filter (comp is-jump? second))
                      (mapcat (fn [[idx op]]
                                (match op
                                  [:jump x] [x]
                                  [:jump-if-zero _ x] [x (inc idx)])))
                      (cons 0)
                      set
                      sort
                      (mapcat
                        (fn [ep]
                          (let [segment (->> (drop ep code)
                                             (reduce (fn [acc op]
                                                       (if (is-jump? op)
                                                         (reduced (conj acc op))
                                                         (conj acc op)))
                                                     []))]
                            [ep (->> segment
                                     (map (fn [op]
                                            (match op
                                              [:return r] (re-get r)
                                              [:load r v] `(aset ~registers (int ~r) (long ~v))
                                              [:loadr to from] `(aset ~registers (int ~to)
                                                                      ~(re-get from))
                                              [:jump-if-zero r to] `(if (zero? ~(re-get r))
                                                                      (recur (int ~to))
                                                                      (recur (int ~(+ ep (count segment)))))
                                              [:jump to] `(recur (int ~to))
                                              [[:bin :add] to r1 r2] `(aset ~registers (int ~to)
                                                                            (unchecked-add
                                                                              ~(re-get r1)
                                                                              ~(re-get r2)))
                                              [[:bin :not=] to r1 r2] `(aset ~registers (int ~to)
                                                                             (if (== ~(re-get r1)
                                                                                     ~(re-get r2))
                                                                               0 1)))))
                                     (cons 'do))]))))
        max-registers (max (->> hoisted keys (reduce max))
                           (->> code
                                (remove (comp #{:jump} first))
                                (map second)
                                (reduce max)))]
    (eval
      `(fn []
         (let [~registers (long-array ~(inc max-registers))]
           (loop [~ip (int 0)]
             (case ~ip
               ~@segments)))))))

(defn registers-loop
  [{:keys [code hoisted]}]
  (let [registers (->> code
                       (remove (comp #{:jump} first))
                       (map second)
                       set
                       sort
                       (map-indexed (fn [idx i]
                                      [i [idx (gensym)]]))
                       (into {}))
        re-get (fn [r] (hoisted r (second (registers r))))
        ip (gensym)
        is-jump? (comp #{:jump :jump-if-zero} first)
        rec (fn [update-ip]
              `(recur ~update-ip
                      ~@(->> registers
                             (map (fn [[i [idx sym]]] [idx sym]))
                             sort
                             (map second))))
        entrypoints (->> code
                         (map-indexed vector)
                         (filter (comp is-jump? second))
                         (mapcat (fn [[idx op]]
                                   (match op
                                     [:jump x] [x]
                                     [:jump-if-zero _ x] [x (inc idx)])))
                         (cons 0)
                         set
                         sort)
        reindex (->> entrypoints
                     (map-indexed vector)
                     (map (comp vec reverse))
                     (into {}))
        segments (->> entrypoints
                      (mapcat
                        (fn [ep]
                          (let [segment (->> (drop ep code)
                                             (reduce (fn [acc op]
                                                       (if (is-jump? op)
                                                         (reduced (conj acc op))
                                                         (conj acc op)))
                                                     []))]
                            [(reindex ep)
                             `(let [~@(->> (butlast segment)
                                           (mapcat (fn [[_ to :as op]]
                                                     [(re-get to)
                                                      (match op
                                                        [:load _ v] v
                                                        [:loadr _ from] (re-get from)
                                                        [[:bin :add] _ r1 r2] `(unchecked-add
                                                                                 ~(re-get r1)
                                                                                 ~(re-get r2))
                                                        [[:bin :not=] _ r1 r2] `(if (== ~(re-get r1)
                                                                                    ~(re-get r2))
                                                                                  0 1))])))]
                                ~(match (last segment)
                                   [:jump to] (rec `(int ~(reindex to)))
                                   [:jump-if-zero r to] (rec `(if (zero? ~(re-get r))
                                                                (int ~(reindex to))
                                                                (int ~(reindex (+ ep (count segment))))))
                                   [:return r] (re-get r)))]))))]
    (eval
      `(fn []
             (loop [~ip (int 0)
                    ~@(->> registers
                           (map (fn [[i [idx sym]]] [idx sym]))
                           sort
                           (mapcat (fn [[_ sym]] [sym `(long 0)])))]
               (case ~ip
                 ~@segments))))))

(defn registers-c
  [{:keys [code hoisted]} iter]
  (let [target-register (fn [op]
                          (match op
                            [:load to _] to
                            [:loadr to _] to
                            [[:bin :add] to _ _] to
                            [[:bin :not=] to _ _] to
                            [:jump _] nil
                            [:jump-if-zero _ _] nil
                            [:return _] nil))
        labels (->> code
                    (keep (fn [op]
                            (case (op 0)
                              :jump (op 1)
                              :jump-if-zero (op 2)
                              nil)))
                    set)
        max-index (max (->> hoisted keys (reduce max))
                       (->> code (keep target-register) (reduce max)))
        lines (fn [s] (->> s (interpose "\n") (apply str)))
        reg (fn [r]
              (or (hoisted r)
                  (str "r_" r)))
        body (->> code
                  (map-indexed
                    (fn [idx op]
                      (str (when (labels idx) (str "LABEL_" idx ":\n"))
                           (match op
                             [:load r v] (str (reg r) " = " v ";")
                             [:loadr to from] (str (reg to) " = " (reg from) ";")
                             [[:bin :add] to arg1 arg2]
                             (str (reg to) " = " (reg arg1) " + " (reg arg2) ";")
                             [[:bin :not=] to arg1 arg2]
                             (str (reg to) " = " (reg arg1) " == " (reg arg2) " ? 0 : 1;")
                             [:jump to] (str "goto LABEL_" to ";")
                             [:jump-if-zero r to] (str "if (" (reg r) " == 0) { goto LABEL_" to "; }")
                             [:return r] (str "return " (reg r) ";")))))
                  lines)
        c-code (lines
                 ["#include <stdio.h>"
                  "#include <time.h>"
                  ""
                  "long compiled_fn(void) {"
                  (->> (range (inc max-index))
                       (remove hoisted)
                       (map (fn [i] (str "long r_" i " = 0;")))
                       lines)
                  body
                  "}"
                  ""
                  "int main() {"
                  "printf(\"%ld\\n\", compiled_fn());"
                  "clock_t start = clock();"
                  (str "for (int i = " iter "; i --> 0; ) {")
                  "compiled_fn();"
                  "}"
                  "printf(\"%ld\\n\", ((clock() - start) * 1000) / CLOCKS_PER_SEC);"
                  "}"
                  ])]
    (let [tmp (-> (shell/sh "mktemp" "-d")
                  :out
                  string/trim)
          _ (spit (str tmp "/main.c") c-code)]
      (->> (shell/sh "bash" "-c" (str "cd " tmp "; cc main.c; ./a.out"))
           :out
           string/trim
           string/split-lines
           (mapv #(Long/parseLong %))))))

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
"5.45e-03"

  (bench (twe-mon ast))

  (def cc (compile-to-closure ast))
  (bench (cc))

  (bench (trampoline (twe-cont ast)))

  (def sc (compile-stack ast))
  (bench (run-stack sc))

  (def rsm (run-stack-mut (compile-stack ast)))
  (bench (rsm))
"6.24e-04"

  (def scc (stack-exec-cont sc))
  (bench (scc))

  (def scm (stack-exec-mut sc))
  (bench (scm))

  (def sca (stack-exec-case sc))
  (bench (sca))

  (def scj (stack-exec-case-jump sc))
  (bench (scj))

  (def rc (compile-register-ssa ast))
  (bench (run-registers rc))

  (def rcj (registers-jump (compile-register-ssa ast)))
  (bench (rcj))

  (def rcj (registers-loop (compile-register-ssa ast)))
  (bench (rcj))

  (registers-c (compile-register-ssa ast) 1000000)

  )
