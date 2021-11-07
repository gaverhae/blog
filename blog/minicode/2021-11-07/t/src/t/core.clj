(ns t.core
  (:require [criterium.core :as crit])
  (:gen-class))

(defmacro bench
  [exp]
  `(prn [(quote ~exp) (->> (crit/benchmark ~exp {}) :mean first (format "%1.2e"))]))

(def integers
  (cons 1
        (lazy-seq (map inc integers))))

(defn divisors
  [n]
  (->> integers
       (take-while (fn [i] (<= i n)))
       (filter (fn [i] (zero? (rem n i))))))

(defn prime?
  [n]
  (= [1 n] (divisors n)))

(def primes
  (filter prime? integers))

(defn get-primes
  []
  (filter prime? integers))

(defn get-primes-sqrt
  []
  (letfn [(divisors [n primes]
            (let [max (Math/sqrt n)]
              (->> primes
                   (take-while (fn [i] (<= i max)))
                   (filter (fn [i] (zero? (rem n i)))))))
          (prime? [n primes]
            (empty? (divisors n primes)))]
    (->> [3 [2] true]
         (iterate (fn [[n primes-so-far added?]]
                    (let [add? (prime? n primes-so-far)]
                      [(inc n)
                       (if add? (conj primes-so-far n) primes-so-far)
                       add?])))
         (filter #(get % 2))
         (map (comp peek second)))))

(defn get-primes-sieve-fn
  []
  (let [sieve (fn sieve [ls]
                (let [p (first ls)]
                  (cons p
                        (lazy-seq (sieve (remove #(zero? (rem % p))
                                                 (rest ls)))))))]
    (cons 2 (sieve (iterate #(+ 2 %) 3)))))

(defn sieve-upto
  [n]
  (loop [candidates (->> (cons 0 (cons 0 (rest integers)))
                         (take-while #(<= % n))
                         vec)
         start 2]
    (let [next-prime (get candidates start)]
      (case next-prime
        nil (vec (remove zero? candidates))
        0 (recur candidates (inc start))
        (recur (let [m (count candidates)]
                 (loop [i (+ start start)
                        c candidates]
                   (if (< i m)
                     (recur (+ i start)
                            (assoc c i 0))
                     c)))
               (inc start))))))

(defn generate-sieve
  [sieve-fn]
  (fn [[idx bound primes prime]]
    (let [idx (inc idx)
          [primes bound] (if (< idx (count primes))
                           [primes bound]
                           (loop [bound (* 2 bound)]
                             (let [primes (sieve-upto bound)]
                               (if (> (count primes) idx)
                                 [primes bound]
                                 (recur (* 2 bound))))))
          prime (get primes idx)]
      [idx bound primes prime])))

(defn get-primes-sieve-vec
  []
  (->> [0 10 [2 3 5 7] 2]
       (iterate (generate-sieve sieve-upto))
       (map peek)))

(defn array-sieve
  [n]
  (let [a (long-array n)]
    (loop [idx 2]
      (when (< idx n)
        (aset-long a idx idx)
        (recur (inc idx))))
    (loop [idx 2]
      (when (< idx n)
        (let [v (aget a idx)]
          (when (not (zero? v))
            (loop [idx2 (* v v)]
              (when (< idx2 n)
                (aset-long a idx2 0)
                (recur (+ v idx2)))))))
      (when (< idx n)
        (recur (inc idx))))
    (vec (remove zero? a))))

(defn get-primes-sieve-arr
  []
  (->> [0 10 [2 3 5 7] 2]
       (iterate (generate-sieve array-sieve))
       (map peek)))

(defn get-primes-final
  []
  (letfn [(h [p0 d bound]
            (concat (drop d p0)
                    (lazy-seq
                      (loop [bound (* 2 bound)]
                        (let [p1 (array-sieve bound)]
                          (if (> (count p1) (count p0))
                            (h p1 (count p0) bound)
                            (recur (* 2 bound))))))))]
    (h [2 3 5 7] 0 10)))



(defn -main
  [& args]
  (bench (nth (get-primes) 100))
  (bench (nth (get-primes) 1000))
  (bench (nth (get-primes-sqrt) 100))
  (bench (nth (get-primes-sqrt) 1000))
  (bench (nth (get-primes-sqrt) 10000))
  (bench (nth (get-primes-sieve-fn) 100))
  (bench (nth (get-primes-sieve-fn) 1000))
  (bench (nth (get-primes-sieve-vec) 1000))
  (bench (nth (get-primes-sieve-vec) 10000))
  (bench (nth (get-primes-sieve-vec) 100000))
  (bench (nth (get-primes-sieve-arr) 1000))
  (bench (nth (get-primes-sieve-arr) 10000))
  (bench (nth (get-primes-sieve-arr) 100000)))
