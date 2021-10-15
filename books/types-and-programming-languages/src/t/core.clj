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

(defn b-3-2-numeric?
  [exp]
  (or (= 0 exp)
      (and (list? exp)
           (= 2 (count exp))
           (= 'succ (first exp))
           (b-3-2-numeric? (second exp)))))

(defn b-3-2-value?
  [exp]
  (or (b-3-1-value? exp)
      (b-3-2-numeric? exp)))

(defn b-3-2-normal?
  [exp]
  (or (b-3-2-value? exp)
      (let [[op t1 t2 t3] exp]
        (case op
          succ (b-3-2-normal? t1)
          pred (and (b-3-2-normal? t1)
                    (not (b-3-2-numeric? t1)))
          zero? (and (b-3-2-normal? t1)
                     (not (b-3-2-numeric? t1)))
          if (and (b-3-2-normal? t1)
                  (b-3-2-numeric? t1))))))

(defn b-3-2-term?
  [exp]
  (or (b-3-2-value? exp)
      (and (list? exp)
           (= 2 (count exp))
           (contains? #{'succ 'pred 'zero?} (first exp))
           (b-3-2-term? (second exp)))
      (and (list? exp)
           (= 4 (count exp))
           (= 'if (first exp))
           (every? b-3-2-term? (rest exp)))))

(defn b-3-2-step
  [exp]
  (case (first exp)
    succ (list 'succ (b-3-2-step (second exp)))
    pred (let [t (second exp)]
            (cond
              (= 0 t) 0
              (and (= 'succ (first t))
                   (b-3-2-numeric? t)) (second t)
              :else (list 'pred (b-3-2-step t))))
    zero? (let [t (second exp)]
             (cond
                   (= 0 t) true
                   (and (= 'succ (first exp))
                        (b-3-2-numeric? (second exp))) false
                   :else (list 'zero? (b-3-2-step t))))
    if (let [[_ t1 t2 t3] exp]
          (case t1
            true t2
            false t3
            (list 'if (b-3-2-step t1) t2 t3)))))

(defn b-3-2-eval
  [exp]
  (cond (b-3-2-value? exp) [:value exp]
        (b-3-2-normal? exp) [:error exp]
        :else (b-3-2-eval (b-3-2-step exp))))
