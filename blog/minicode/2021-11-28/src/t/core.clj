(ns t.core
  (:gen-class)
  (:require [criterium.core :as crit]))

(defn check-based
  []
  (let [p (promise)]
    (deliver p (cons 2
                     (->> (iterate #(+ 2 %) 3)
                          (remove (fn [n]
                                    (->> @p
                                         (take-while #(<= (* % %) n))
                                         (some #(zero? (rem n %)))))))))
    @p))

(defn check-based-with-bigint
  [certainty]
  (let [p (promise)]
    (deliver p (cons 2
                     (->> (iterate #(+ 2 %) 3)
                          (filter (fn [n]
                                    (.isProbablePrime
                                      (java.math.BigInteger/valueOf n)
                                      certainty)))
                          (remove (fn [n]
                                    (->> @p
                                         (take-while #(<= (* % %) n))
                                         (some #(zero? (rem n %)))))))))
    @p))

(defn sieve-based
  [sieve]
  (letfn [(helper [known-primes already-produced sieve-bound]
            (concat (drop already-produced known-primes)
                    (lazy-seq
                      (loop [new-bound (* 10 sieve-bound)]
                        (let [new-primes (sieve new-bound)]
                          (if (> (count new-primes) (count known-primes))
                            (helper new-primes (count known-primes) new-bound)
                            (recur (* 10 new-bound))))))))]
    (helper (sieve 10) 0 10)))

(defn array-sieve
  [bound]
  (let [candidates (long-array bound)]
    ;; initialize array to contain [0 0 2 3 4 5 ...]
    (loop [idx 2]
      (when (< idx bound)
        (aset-long candidates idx idx)
        (recur (inc idx))))
    ;; eliminate known non-primes
    (loop [idx 2]
      (when (< idx bound)
        (let [next-candidate (aget candidates idx)]
          (when-not (zero? next-candidate)
            (loop [update-idx (* idx idx)]
              (when (< update-idx bound)
                (aset-long candidates update-idx 0)
                (recur (+ idx update-idx)))))
          (recur (inc idx)))))
    ;; return a Clojure vector with only primes in it
    (vec (remove zero? candidates))))

(defn bitset-sieve
  [^long bound]
  (let [candidates (java.util.BitSet. bound)]
    ;; initialize array to contain [0 0 2 3 4 5 ...]
    (.set candidates 2 bound)
    ;; eliminate known non-primes
    (loop [idx 2]
      (when (< idx bound)
        (when-let [next-candidate (.get candidates idx)]
          (loop [update-idx (* idx idx)]
            (when (< update-idx bound)
              (.clear candidates update-idx)
              (recur (+ idx update-idx)))))
        (recur (inc idx))))
    ;; return a Clojure seq with only primes in it
    (take-while pos? (iterate #(.nextSetBit candidates (inc %)) 2))))

(defmacro bench
  [primes-fn]
  `(doseq [n# [100000 300000 1000000]]
     (println (format "%-28s %7d: %5.2f"
                      (pr-str '~primes-fn)
                      n#
                      (->> (crit/quick-benchmark (nth ~primes-fn n#) {})
                           :mean first)))))

(defn -main
  [& args]
  (bench (check-based))
  (bench (check-based-with-bigint 2))
  (bench (check-based-with-bigint 5))
  (bench (sieve-based array-sieve))
  (bench (sieve-based bitset-sieve)))
