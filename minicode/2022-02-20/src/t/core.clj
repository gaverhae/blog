(ns t.core
  (:require [criterium.core :as crit])
  (:gen-class))

(defn bench
  [f]
  (->> (crit/benchmark (f) {}) :mean first))

(defn char-at
  [s idx]
  (.charAt s idx))

(defn fast-char-at
  [^String s ^long idx]
  (.charAt s idx))

(defn -main
  [& args]
  (let [r1 (char-at "hello" 2)
        r2 (fast-char-at "hello" 2)
        t1 (bench #(char-at "hello" 2))
        t2 (bench #(fast-char-at "hello" 2))]
    (println (format "%-15s: %.2e (%c)" "char-at" t1 r1))
    (println (format "%-15s: %.2e (%c)" "fast-char-at" t2 r2))
    (println (format "speedup: %6.2f" (/ t1 t2)))))
