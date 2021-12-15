(ns t.core
  (:gen-class)
  (:import [java.util PriorityQueue])
  (:import [java.util BitSet])
  (:require [criterium.core :as crit]))

;(comment
(set! *unchecked-math* :warn-on-boxed)

(defn array-sieve
  [^long bound]
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
  (let [candidates (java.util.BitSet. bound)
        up (inc (long (Math/sqrt bound)))]
    ;; initialize array to contain [0 0 2 3 4 5 ...]
    (.set candidates 2 bound)
    ;; special-case 2 so we can increment by 2 afterwards
    (loop [update-idx 4]
      (when (< update-idx bound)
        (.clear candidates update-idx)
        (recur (+ 2 update-idx))))
    ;; eliminate known non-primes
    (loop [idx 3]
      (when (< idx up)
        (when-let [next-candidate (.get candidates idx)]
          (loop [update-idx (* idx idx)]
            (when (< update-idx bound)
              (.clear candidates update-idx)
              (recur (+ idx update-idx)))))
        (recur (+ 2 idx))))
    ;; return a Clojure seq with only primes in it
    #_(take-while pos? (iterate (fn [^long x] (.nextSetBit candidates (inc x))) 2))
    (let [size ^int (.cardinality candidates)
          arr (int-array size)]
      (loop [idx (int 0)
             iter (int 0)]
        (if (= iter size)
          (seq arr)
          (let [v (.nextSetBit candidates idx)]
            (aset arr iter v)
            (recur (inc v) (inc iter))))))))

(defn just-bitset-sieve
  [^long bound]
  (let [candidates (java.util.BitSet. bound)
        up (inc (long (Math/sqrt bound)))]
    ;; initialize array to contain [0 0 2 3 4 5 ...]
    (.set candidates 2 bound)
    ;; special-case 2 so we can increment by 2 afterwards
    (loop [update-idx 4]
      (when (< update-idx bound)
        (.clear candidates update-idx)
        (recur (+ 2 update-idx))))
    ;; eliminate known non-primes
    (loop [idx 3]
      (when (< idx up)
        (when-let [next-candidate (.get candidates idx)]
          (loop [update-idx (* idx idx)]
            (when (< update-idx bound)
              (.clear candidates update-idx)
              (recur (+ idx update-idx)))))
        (recur (+ 2 idx))))
    ;; return a Clojure seq with only primes in it
    []))

(let [bs (memoize (fn [^long bound]
                    (let [candidates (java.util.BitSet. bound)
                          up (inc (long (Math/sqrt bound)))]
                      ;; initialize array to contain [0 0 2 3 4 5 ...]
                      (.set candidates 2 bound)
                      ;; special-case 2 so we can increment by 2 afterwards
                      (loop [update-idx 4]
                        (when (< update-idx bound)
                          (.clear candidates update-idx)
                          (recur (+ 2 update-idx))))
                      ;; eliminate known non-primes
                      (loop [idx 3]
                        (when (< idx up)
                          (when-let [next-candidate (.get candidates idx)]
                            (loop [update-idx (* idx idx)]
                              (when (< update-idx bound)
                                (.clear candidates update-idx)
                                (recur (+ idx update-idx)))))
                          (recur (+ 2 idx))))
                      candidates)))]
  (defn after-bitset-sieve
    [^long bound]
    (let [candidates ^BitSet (bs bound)]
      #_(take-while pos? (iterate (fn [^long x] (.nextSetBit candidates (inc x))) 2))
      (let [size ^int (.cardinality candidates)
            arr (int-array size)]
        (loop [idx (int 0)
               iter (int 0)]
          (if (= iter size)
            (seq arr)
            (let [v (.nextSetBit candidates idx)]
            (aset arr iter v)
            (recur (inc v) (inc iter)))))))))

(defmacro halve
  "Efficiently halve a number."
  [n]
  `(bit-shift-right ~n 1))

(defmacro doubl
  "Efficiently double a number."
  [n]
  `(bit-shift-left ~n 1))

(defn sieve-1bit
  [init? ^long limit]
  (let [q (inc (Math/sqrt limit))
        sieve (if init?
                (java.util.BitSet. limit)
                (java.util.BitSet.))]
    ;; Highly optimised Sieve of Eratosthenes algorithm.
    (loop [factor 3]
      (when (< factor q)
        (if-not (.get sieve (halve factor))
          (let [factor*2 (doubl factor)]
            (loop [num (* factor factor)]
              (when (<= num limit)
                (.set sieve (halve num) true)
                (recur (+ num factor*2))))))
        (recur (+ 2 factor))))
    ;; Return sequence of found prime numbers.
    (cons 2 (->> (range 3 limit 2)
                 (map (fn [^long n]
                        (if-not (.get sieve (halve n)) n)))
                 (filter some?)))))

(let [bs (memoize (fn [init? ^long limit]
                    (let [q (inc (Math/sqrt limit))
                          sieve (if init?
                                  (java.util.BitSet. limit)
                                  (java.util.BitSet.))]
                      ;; Highly optimised Sieve of Eratosthenes algorithm.
                      (loop [factor 3]
                        (when (< factor q)
                          (if-not (.get sieve (halve factor))
                            (let [factor*2 (doubl factor)]
                              (loop [num (* factor factor)]
                                (when (<= num limit)
                                  (.set sieve (halve num) true)
                                  (recur (+ num factor*2))))))
                          (recur (+ 2 factor))))
                      sieve)))]
  (defn after-sieve-1bit
    [init? ^long limit]
    (let [sieve ^BitSet (bs init? limit)]
      (cons 2 (->> (range 3 limit 2)
                   (map (fn [^long n]
                          (if-not (.get sieve (halve n)) n)))
                   (filter some?))))))

(defn just-sieve-1bit
  [init? ^long limit]
  (let [q (inc (Math/sqrt limit))
        sieve (if init?
                (java.util.BitSet. limit)
                (java.util.BitSet.))]
    ;; Highly optimised Sieve of Eratosthenes algorithm.
    (loop [factor 3]
      (when (< factor q)
        (if-not (.get sieve (halve factor))
          (let [factor*2 (doubl factor)]
            (loop [num (* factor factor)]
              (when (<= num limit)
                (.set sieve (halve num) true)
                (recur (+ num factor*2))))))
        (recur (+ 2 factor))))
    ;; Return sequence of found prime numbers.
    []))

(defn sieve-8bit
  [^long limit]
  (let [q (inc (Math/sqrt limit))
        ^booleans sieve (make-array Boolean/TYPE (halve limit))]
    ;; Highly optimised Sieve of Eratosthenes algorithm.
    (loop [factor 3]
      (when (< factor q)
        (if-not (aget sieve (halve factor))
          (let [factor*2 (doubl factor)]
            (loop [num (* factor factor)]
              (when (<= num limit)
                (aset sieve (halve num) true)
                (recur (+ num factor*2))))))
        (recur (+ 2 factor))))
    ;; Return sequence of found prime numbers.
    (cons 2 (->> (range 3 limit 2)
                 (map (fn [^long n]
                        (if-not (aget sieve (halve n)) n)))
                 (filter some?)))))

(defn just-sieve-8bit
  [^long limit]
  (let [q (inc (Math/sqrt limit))
        ^booleans sieve (make-array Boolean/TYPE (halve limit))]
    ;; Highly optimised Sieve of Eratosthenes algorithm.
    (loop [factor 3]
      (when (< factor q)
        (if-not (aget sieve (halve factor))
          (let [factor*2 (doubl factor)]
            (loop [num (* factor factor)]
              (when (<= num limit)
                (aset sieve (halve num) true)
                (recur (+ num factor*2))))))
        (recur (+ 2 factor))))
    ;; Return sequence of found prime numbers.
    []))

(let [bs (memoize (fn [^long limit]
                    (let [q (inc (Math/sqrt limit))
                          ^booleans sieve (make-array Boolean/TYPE (halve limit))]
                      ;; Highly optimised Sieve of Eratosthenes algorithm.
                      (loop [factor 3]
                        (when (< factor q)
                          (if-not (aget sieve (halve factor))
                            (let [factor*2 (doubl factor)]
                              (loop [num (* factor factor)]
                                (when (<= num limit)
                                  (aset sieve (halve num) true)
                                  (recur (+ num factor*2))))))
                          (recur (+ 2 factor))))
                      sieve)))]
  (defn after-sieve-8bit
    [^long limit]
    (let [^booleans sieve (bs limit)]
      (cons 2 (->> (range 3 limit 2)
                   (map (fn [^long n]
                          (if-not (aget sieve (halve n)) n)))
                   (filter some?))))))

(defn bench
  [primes-fn label]
  (print (format "[%-18s" label))
  (flush)
  (doseq [n [1000 3000
             (* 10 1000) (* 30 1000)
             (* 100 1000) (* 300 1000)
             (* 1000 1000) (* 3000 1000)
             (* 10 1000 1000) (* 30 1000 1000)
             (* 100 1000 1000) (* 300 1000 1000)]]
    (print (format " %6.3f"
                   (-> (crit/benchmark (count (primes-fn n)) {})
                       :mean first)))
    (flush))
  (println "]"))

(defn -main
  [& args]
  (let [limit (* 30 1000 1000)]
    (prn (= #_(array-sieve limit)
            (bitset-sieve limit)
            (after-bitset-sieve limit)
            #_(sieve-1bit false limit)
            #_(sieve-1bit true limit)
            #_(sieve-8bit limit))))
  #_(bench #(array-sieve %) :my-array)
  (bench #(bitset-sieve %) :my-bitset)
  #_(bench #(just-bitset-sieve %) :just-my-bitset)
  (bench #(after-bitset-sieve %) :after-my-bitset)
  #_(bench #(sieve-1bit false %) :1bit)
  #_(bench #(just-sieve-1bit false %) :just-1bit)
  #_(bench #(after-sieve-1bit false %) :after-1bit)
  #_(bench #(sieve-1bit true %) :1bit-init)
  #_(bench #(just-sieve-1bit true %) :just-1bit-init)
  #_(bench #(after-sieve-1bit true %) :after-1bit-init)
  #_(bench #(sieve-8bit %) :8bit)
  #_(bench #(just-sieve-8bit %) :just-8bit)
  #_(bench #(after-sieve-8bit %) :after-8bit))

(set! *unchecked-math* false)
;)

(comment
(ns t.core
  (:gen-class)
  (:import [java.util PriorityQueue])
  (:require [criterium.core :as crit]))

(set! *unchecked-math* :warn-on-boxed)

(defn unfaithful
  ([] (unfaithful (iterate inc 2)))
  ([[p & xs]] (cons p (lazy-seq (unfaithful (filter (fn [n] (pos? ^long (mod n p))) xs))))))

(defn trial-division
  []
  (let [primes (promise)
        prime? (fn [x]
                 (->> @primes
                      (take-while (fn [^long x] (<= (* x x) x)))
                      (every? #(pos? ^long (mod x %)))))]
    (deliver primes (cons 2 (filter prime? (iterate inc 3))))
    @primes))

(defn sieve
  ([s] (sieve s {}))
  ([[^long x & xs] table]
   (if-let [factors (get table x)]
     (sieve xs (reduce (fn [t ^long prime]
                         (update t (+ x prime) concat [prime]))
                       (dissoc table x)
                       factors))
     (cons x (lazy-seq (sieve xs (assoc table (* x x) [x])))))))

(let [insert-prime (fn [table ^long x xs]
                     (assoc table (* x x) [(map (fn [^long y] (* x y)) xs)]))]
  (defn sieve-sm
    ([[i & is]] (sieve-sm is (insert-prime (sorted-map) i is)))
    ([[^long x & xs] table]
     (let [[^long next-composite factors] (first table)]
       (if (> next-composite x) ;; x is prime
         (cons x (lazy-seq (sieve-sm xs (insert-prime table x xs))))
         (sieve-sm xs (reduce (fn [table [next-comp & future-comps]]
                                (update table next-comp
                                        conj future-comps))
                              (dissoc table next-composite)
                              factors)))))))

(let [insert-prime (fn [^PriorityQueue table ^long x xs]
                     (.add table [(* x x) (map (fn [^long y] (* x y)) xs)])
                     table)]
  (defn sieve-pq
    ([[i & is]] (sieve-pq
                  is
                  (insert-prime
                    (PriorityQueue. 10 (fn [[^long x] [^long y]] (< x y)))
                    i is)))
    ([[^long x & xs] ^PriorityQueue table]
     (let [[^long next-composite] (.peek table)]
       (if (> next-composite x)
         (cons x (lazy-seq (sieve-pq xs (insert-prime table x xs))))
         (do (while (== x ^long (first (.peek table)))
               (let [[_ [f & fs]] (.poll table)]
                 (.add table [f fs])))
             (sieve-pq xs table)))))))

(def wheel2357
  (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2 6 4 6 8 4 2 4 2 4 8
          6 4 6 2 4 6 2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10]))

(defn spin
  [[^long x & xs] ^long n]
  (cons n (lazy-seq (spin xs (+ n x)))))

(defn spin-primes
  [sieve-fn]
  (concat [2 3 5 7]
          (sieve-fn (spin wheel2357 11))))

(defn sieve-based
  [sieve]
  (letfn [(helper [known-primes already-produced ^long sieve-bound]
            (concat (drop already-produced known-primes)
                    (lazy-seq
                      (loop [new-bound ^long (* 10 sieve-bound)]
                        (let [new-primes (sieve new-bound)]
                          (if (> (count new-primes) (count known-primes))
                            (helper new-primes (count known-primes) new-bound)
                            (recur (* 10 new-bound))))))))]
    (helper (sieve 10) 0 10)))

(defn array-sieve
  [^long bound]
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
    (take-while pos? (iterate (fn [^long x] (.nextSetBit candidates (inc x))) 2))))

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

(defmacro halve
  "Efficiently halve a number."
  [n]
  `(bit-shift-right ~n 1))

(defmacro doubl
  "Efficiently double a number."
  [n]
  `(bit-shift-left ~n 1))

(defn sieve-1bit
  [^long limit init?]
  (let [q (inc (Math/sqrt limit))
        sieve (if init?
                (java.util.BitSet. limit)
                (java.util.BitSet.))]
    ;; Highly optimised Sieve of Eratosthenes algorithm.
    (loop [factor 3]
      (when (< factor q)
        (if-not (.get sieve (halve factor))
          (let [factor*2 (doubl factor)]
            (loop [num (* factor factor)]
              (when (<= num limit)
                (.set sieve (halve num) true)
                (recur (+ num factor*2))))))
        (recur (+ 2 factor))))
    ;; Return sequence of found prime numbers.
    (cons 2 (->> (range 3 limit 2)
                 (map (fn [^long n]
                        (if-not (.get sieve (halve n)) n)))
                 (filter some?)))))

(defn sieve-8bit
  [^long limit]
  (let [q (inc (Math/sqrt limit))
        ^booleans sieve (make-array Boolean/TYPE (halve limit))]
    ;; Highly optimised Sieve of Eratosthenes algorithm.
    (loop [factor 3]
      (when (< factor q)
        (if-not (aget sieve (halve factor))
          (let [factor*2 (doubl factor)]
            (loop [num (* factor factor)]
              (when (<= num limit)
                (aset sieve (halve num) true)
                (recur (+ num factor*2))))))
        (recur (+ 2 factor))))
    ;; Return sequence of found prime numbers.
    (cons 2 (->> (range 3 limit 2)
                 (map (fn [^long n]
                        (if-not (aget sieve (halve n)) n)))
                 (filter some?)))))

(defn -main
  [& args]
  #_(bench #(bitset-sieve 100000000) :mine)
  (bench #(sieve-1bit 100000000 false) :1bit)
  (bench #(sieve-1bit 100000000 true) :1bit)
  #_(bench #(sieve-8bit 100000000) :8bit)
  #_(bench #(trial-division) :trial-division)
  #_(bench #(sieve (iterate inc 2)) :sieve)
  #_(bench #(sieve-sm (iterate inc 2)) :sieve-sm)
  #_(bench #(sieve-pq (iterate inc 2)) :sieve-pq)
  #_(bench #(spin-primes sieve-sm) :spin-sm)
  #_(bench #(spin-primes sieve-pq) :spin-pq)
  #_(bench #(sieve-based array-sieve) :array-sieve)
  #_(bench #(sieve-based bitset-sieve) :bitset-sieve))

(set! *unchecked-math* false)
)
