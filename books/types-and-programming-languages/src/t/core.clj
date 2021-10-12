(ns t.core)

(defn value-3-1
  [exp]
  (contains? #{true false} exp))

(defn term-3-1
  [exp]
  (or (value-3-1 exp)
      (and (list? exp)
           (= 4 (count exp))
           (= 'if (first exp))
           (every? term-3-1 (rest exp)))))

(defn step-3-1
  [exp]
  (cond
    (value-3-1 exp) [:value exp]
    (not (term-3-1 exp)) [:not-a-term exp]
    :else
    (let [[_ t1 t2 t3] exp]
      (case t1
        true [:if-true t2]
        false [:if-false t3]
        (let [[r t] (step-3-1 t1)]
          (prn r)
          [:if (list 'if t t2 t3)])))))
