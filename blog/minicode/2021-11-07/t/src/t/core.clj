(ns t.core
  (:require [criterium.core :as crit])
  (:gen-class))

(defmacro bench
  [exp]
  `(->> (crit/benchmark ~exp {}) :mean first (format "%1.2e")))

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

(defn nth-prime
  [n]
  (->> primes
       (drop n)
       first))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
