(ns t.core)

(defn b-3-1-value?
  [exp]
  (contains? #{true false} exp))

(defn b-3-1-term?
  [exp]
  (or (b-3-1-value? exp)
      (and (list? exp)
           (= 4 (count exp))
           (= 'if (first exp))
           (every? b-3-1-term? (rest exp)))))

(def b-3-1-normal?
  b-3-1-value?)

(defn b-3-1-step
  [[_ t1 t2 t3]]
  (case t1
    true t2
    false t3
    (list 'if (b-3-1-step t1) t2 t3)))

(defn b-3-1-eval
  [exp]
  (cond (b-3-1-value? exp) [:value exp]
        (b-3-1-normal? exp) [:error exp]
        :else (b-3-1-eval (b-3-1-step exp))))
