(ns t.core
  (:gen-class))

(defn sieve
  ([s] (sieve s {}))
  ([[x & xs] table]
   (if-let [factors (get table x)]
     (sieve xs (reduce (fn [t prime]
                         (update t (+ x prime) concat [prime]))
                       (dissoc table x)
                       factors))
     (cons x (lazy-seq (sieve xs (assoc table (* x x) [x])))))))

(def primes
  (sieve (iterate inc 2)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
