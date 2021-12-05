(ns t.core
  (:gen-class)
  (:require [criterium.core :as crit]))

(defn unfaithful
  ([] (unfaithful (iterate inc 2)))
  ([[p & xs]] (cons p (lazy-seq (unfaithful (filter #(pos? (mod % p)) xs))))))

(defn trial-division
  []
  (let [primes (promise)
        prime? (fn [x]
                 (->> @primes
                      (take-while #(<= (* % %) x))
                      (every? #(pos? (mod x %)))))]
    (deliver primes (cons 2 (filter prime? (iterate inc 3))))
    @primes))

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

(defn bench
  [primes-fn label sizes]
  (let [results (->> sizes
                     (map (fn [s]
                            (format "%6.3f"
                                    (-> (crit/benchmark (nth (primes-fn) s) {})
                                        :mean first))))
                     (interpose " ")
                     (apply str))]
    (println (format "[%-15s %s]" label results))))

(defn -main
  [& args]
  (bench #(trial-division) :trial-division)
  (bench #(sieve-based array-sieve) :array-sieve)
  (bench #(sieve-based bitset-sieve) :bitset-sieve))
