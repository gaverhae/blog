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
      (recur (+ total steps) (dec steps)))))

(defn unchecked
  []
  (loop [total 0, steps 1000000]
    (if (zero? steps)
      total
      (recur (unchecked-add total steps) (unchecked-dec steps)))))

;; this is the correct place to set the flag
(set! *unchecked-math* true)
(defn also-unchecked
  []
  (loop [total 0, steps 1000000]
    (if (zero? steps)
      total
      (recur (+ total steps) (dec steps)))))
(set! *unchecked-math* false)

(defn -main
  [& args]
  (doseq [[n f] [["checked" checked]
                 ["unchecked" unchecked]
                 ["also-unchecked" also-unchecked]]]
    (println (format "%-15s: %.2e (%d)" n (bench f) (f)))))
