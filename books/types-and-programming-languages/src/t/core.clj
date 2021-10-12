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


;(def lang-3-1
;  {:values [true false]
;   :terms ['(if :t :t :t)]
;   :eval {:if-true ['(if true :t1 :t2) :t1]
;          :if-false ['(if false :t1 :t2) :t2]
;          :if ['(if :t1 :t2 :t3)
;               '(if [:t1] :t2 :t3)]}})
;
;(defn match?
;  [term? pat exp]
;  (or (= pat exp)
;      (and (list? pat)
;           (list? exp)
;           (= (count exp) (count pat))
;           (every? (fn [[pat exp]] (match? term? pat exp))
;                   (map vector pat exp)))
;      (and (keyword? pat)
;           (term? exp))))
;
;(defn value?
;  [lang exp]
;  (some #(match? (constantly false) % exp) (:values lang)))
;
;(defn term?
;  [lang exp]
;  (or (value? lang exp)
;      (some #(match? (partial term? lang) % exp) (:terms lang))))
;
;(defn step
;  [lang exp]
;  (cond
;    (not (term? lang exp)) [:not-a-term]
;    (value? lang exp) [:value exp]
;    :else
;    (let [match? (partial match? (partial term? lang))
;          matching-rules (->> (:eval lang)
;                              (filter (fn [[_ [pat _]]] (match? pat exp))))]
;      (if (not= 1 (count matching-rules))
;        [:bad-language matching-rules exp]
;
;
