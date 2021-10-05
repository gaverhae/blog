(ns t.core
  (:refer-clojure :exclude [update]))

(defmacro match
  [expr & cases]
  (let [e (gensym)]
    `(let [~e ~expr]
       (case (first ~e)
         ~@(->> (partition 2 cases)
                (mapcat (fn [[pat body]]
                          [(first pat) `(let [~(vec (cons '_ (rest pat))) ~e]
                                          ~body)])))))))

(defmacro mdo
  [bindings]
  (if (#{0 1} (count bindings))
    (throw (RuntimeException. "invalid number of elements in mdo bindings"))
    (let [[n v & r] bindings]
      (if (empty? r)
        v
        [:bind v `(fn [~n] (mdo ~r))]))))

(defn run-ambient
  ([m] (run-ambient m {}))
  ([m env]
   (match m
     [:return v] [v env]
     [:bind ma f] (let [[v env] (run-ambient ma env)]
                    (run-ambient (f v) env))
     [:set k v] [v (assoc env k v)]
     [:get k] [(get env k) env])))

(comment
(run-ambient
  (mdo [a [:return 5]
        b [:return 3]
        c [:return (+ a b)]
        _ [:set 0 c]
        v [:get 2]
        _ [:return (+ v c)]])
  {2 3})
[11 {2 3, 0 8}]
  )

(defn sequenceM
  "Takes a list of monadic values ms, and returns a single monadic value that
   wraps a list."
  [ms]
  (if (empty? ms)
    [:return ()]
    (mdo [a (first ms)
          r (sequenceM (rest ms))
          _ [:return (cons a r)]])))

(defn update
  [k f]
  (mdo [v [:get k]
        _ [:set k (f v)]]))

(defn count-exprs
  [expr]
  (let [finc (fnil inc 0)]
    (match expr
      [:lit e] (mdo [_ (update :lit finc)
                     _ [:return 1]])
      [:var _] (mdo [_ (update :var finc)
                     _ [:return 1]])
      [:set _ e] (mdo [c (count-exprs e)
                       _ (update :set finc)
                       _ [:return (inc c)]])
      [:bin _ e1 e2] (mdo [c1 (count-exprs e1)
                           c2 (count-exprs e2)
                           _ (update :bin finc)
                           _ [:return (+ c1 c2 1)]])
      [:while e-cond e-body] (mdo [c1 (count-exprs e-cond)
                                   c2 (count-exprs e-body)
                                   _ (update :while finc)
                                   _ [:return (+ c1 c2 1)]])
      [:do & exprs] (mdo [counts (sequenceM (mapv count-exprs exprs))
                          _ (update :do finc)
                          _ [:return (reduce + 1 counts)]]))))

(comment
(run-ambient
  (count-exprs
    [:do
     [:set 0 [:lit 100]]
     [:set 1 [:lit 1000]]
     [:while
      [:bin :not= [:lit 0] [:var 1]]
      [:do
       [:set 0 [:bin :add [:bin :add [:bin :add [:var 0] [:lit 4]]
                           [:var 0]]
                [:lit 3]]]
       [:set 0 [:bin :add [:bin :add [:var 0] [:lit 2]]
                [:lit 4]]]
       [:set 1 [:bin :add [:lit -1] [:var 1]]]]]
     [:var 0]]))
[29 {:lit 8, :set 5, :var 6, :bin 7, :do 2, :while 1}]
)

(defn run-nd
  ([ma] (run-nd ma []))
  ([ma s]
   (match ma
     [:return a] [a]
     [:bind ma f] (mapcat (comp run-nd f) (run-nd ma))
     [:multi ls] ls)))


(comment
(run-nd (mdo [a [:multi [1 2]]
              b [:multi [3 4]]
              _ [:return (+ a b)]]))
(4 5 5 6)
)

(defn filter-pos
  [a]
  [:multi (if (pos? a) [a] [])])

(comment
(run-nd (mdo [a [:multi [-1 2]]
              b [:multi [3 4]]
              c [:return (* a b)]
              _ (filter-pos c)]))
)

(defn sqrt
  [x]
  (cond (neg? x)  [:multi []]
        (zero? x) [:return 0]
        (pos? x)  (mdo [sign [:multi [1 -1]]
                        _    [:return (* sign (Math/sqrt x))]])))

(defn div
  [a b]
  (if (zero? b)
    [:multi []]
    [:return (/ a b)]))

(defn solve-2nd
  [a b c]
  (mdo [d (sqrt (- (* b b) (* 4 a c)))
        x (div (- d b)
               (* 2 a))]))

(comment
(run-nd (solve-2nd 1 2 1))
(-1)
(run-nd (solve-2nd 1 3 1))
(-0.3819660112501051 -2.618033988749895)
(run-nd (solve-2nd -2 0 -1))
()
)
