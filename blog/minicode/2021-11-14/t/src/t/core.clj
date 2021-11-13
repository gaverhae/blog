(ns t.core)

(defn promise-example
  []
  (let [p (promise)
        t1 (Thread. (fn []
                      (dotimes [n 5]
                        (Thread/sleep 100)
                        (print (str "T1: " @p "\n"))
                        (flush))))
        t2 (Thread. (fn []
                      (dotimes [n 5]
                        (Thread/sleep 50)
                        (print (str "T2: " (deref p 50 :timeout) "\n"))
                        (flush))))]
    (.start t1)
    (.start t2)
    (println (realized? p))
    (Thread/sleep 300)
    (deliver p 42)
    (println (realized? p))
    (.join t1)
    (.join t2)
    :done))

(def primes
  (cons 2
        (->> (iterate (fn [i] (+ 2 i)) 3)
             (remove (fn [n]
                       (->> primes
                            (take-while (fn [p] (<= (* p p) n)))
                            (some (fn [d] (zero? (rem n d))))))))))

(defn get-primes
  []
  (cons 2
        (->> (iterate (fn [i] (+ 2 i)) 3)
             (remove (fn [n]
                       (->> (get-primes)
                            (take-while (fn [p] (<= (* p p) n)))
                            (some (fn [d] (zero? (rem n d))))))))))

(def get-primes-memo
  (let [h (fn [p]
            (cons 2
                  (->> (iterate (fn [i] (+ 2 i)) 3)
                       (remove (fn [n]
                                 (->> (p)
                                      (take-while (fn [p] (<= (* p p) n)))
                                      (some (fn [d] (zero? (rem n d))))))))))]
    (fn [] (h (memoize get-primes-memo)))))

(defn get-primes-promise
  []
  (let [p (promise)]
    (deliver p (cons 2
                     (->> (iterate (fn [i] (+ 2 i)) 3)
                          (remove (fn [n]
                                    (->> @p
                                         (take-while (fn [p] (<= (* p p) n)))
                                         (some (fn [d] (zero? (rem n d))))))))))
    @p))
