(ns t.core
  (:require [criterium.core :as crit])
  (:gen-class))

(defn bench
  [f]
  (->> (crit/benchmark (f) {}) :mean first))

(defn checked
  []
  (loop [total 0, steps 1000000]
    (if (zero? steps)
      total
      (recur (+ total steps)
             (dec steps)))))

(defn unchecked
  []
  (loop [total 0, steps 1000000]
    (if (zero? steps)
      total
      (recur (unchecked-add total steps)
             (unchecked-dec steps)))))

;; this is the correct place to set the flag
(set! *unchecked-math* true)
(defn also-unchecked
  []
  (loop [total 0, steps 1000000]
    (if (zero? steps)
      total
      (recur (+ total steps)
             (dec steps)))))
(set! *unchecked-math* false)

(defn boxed-add
  [a b]
  (+ a b))

(defn boxed
  []
  (loop [total 0, steps 1000000]
    (if (zero? steps)
      total
      (recur (boxed-add total steps)
             (dec steps)))))

(defn non-boxed-add
  ^long [^long a ^long b]
  (+ a b))

(defn non-boxed
  []
  (loop [total 0, steps 1000000]
    (if (zero? steps)
      total
      (recur (non-boxed-add total steps)
             (dec steps)))))

(defn non-boxed-unchecked-add
  ^long [^long a ^long b]
  (unchecked-add a b))

(defn non-boxed-unchecked
  []
  (loop [total 0, steps 1000000]
    (if (zero? steps)
      total
      (recur (non-boxed-unchecked-add total steps)
             (unchecked-dec steps)))))

(defn -main
  [& args]
  (doseq [[n f] [#_["checked" checked]
                 #_["unchecked" unchecked]
                 #_["also-unchecked" also-unchecked]
                 ["boxed" boxed]
                 ["non-boxed" non-boxed]
                 ["non-boxed-unchecked" non-boxed-unchecked]]]
    (println (format "%-15s: %.2e (%d)" n (bench f) (f)))))
