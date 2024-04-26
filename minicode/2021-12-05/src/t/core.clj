(ns t.core
  (:gen-class)
  (:import [java.util PriorityQueue])
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

(let [insert-prime (fn [table x xs]
                     (assoc table (* x x) [(map #(* x %) xs)]))]
  (defn sieve-sm
    ([[i & is]] (sieve-sm is (insert-prime (sorted-map) i is)))
    ([[x & xs] table]
     (let [[next-composite factors] (first table)]
       (if (> next-composite x) ;; x is prime
         (cons x (lazy-seq (sieve-sm xs (insert-prime table x xs))))
         (sieve-sm xs (reduce (fn [table [next-comp & future-comps]]
                                (update table next-comp
                                        conj future-comps))
                              (dissoc table next-composite)
                              factors)))))))

(let [insert-prime (fn [^PriorityQueue table x xs]
                     (.add table [(* x x) (map #(* x %) xs)])
                     table)]
  (defn sieve-pq
    ([[i & is]] (sieve-pq
                  is
                  (insert-prime
                    (PriorityQueue. 10 (fn [[x] [y]] (< x y)))
                    i is)))
    ([[x & xs] ^PriorityQueue table]
     (let [[next-composite] (.peek table)]
       (if (> next-composite x)
         (cons x (lazy-seq (sieve-pq xs (insert-prime table x xs))))
         (do (while (== x (first (.peek table)))
               (let [[_ [f & fs]] (.poll table)]
                 (.add table [f fs])))
             (sieve-pq xs table)))))))

(def wheel2357
  (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2 6 4 6 8 4 2 4 2 4 8
          6 4 6 2 4 6 2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10]))

(defn spin
  [[x & xs] n]
  (cons n (lazy-seq (spin xs (+ n x)))))

(defn spin-primes
  [sieve-fn]
  (concat [2 3 5 7]
          (sieve-fn (spin wheel2357 11))))

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
  [primes-fn label]
  (let [results (->> [1000 3000 10000 30000 100000]
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
  (bench #(sieve (iterate inc 2)) :sieve)
  (bench #(sieve-sm (iterate inc 2)) :sieve-sm)
  (bench #(sieve-pq (iterate inc 2)) :sieve-pq)
  (bench #(spin-primes sieve-sm) :spin-sm)
  (bench #(spin-primes sieve-pq) :spin-pq)
  (bench #(sieve-based array-sieve) :array-sieve)
  (bench #(sieve-based bitset-sieve) :bitset-sieve))
