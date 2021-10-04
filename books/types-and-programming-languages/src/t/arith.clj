(ns t.arith
  (:require [t.lib :refer [match]]
            [clojure.set :as set]))

(defn consts
  [e]
  (match e
    [:true] #{true}
    [:false] #{false}
    [:zero] #{0}
    [:succ n] (consts n)
    [:pred n] (consts n)
    [:iszero n] (consts n)
    [:if c r1 r2] (set/union (consts c)
                             (consts r1)
                             (consts r2))))

(defn size
  [e]
  (match e
    [:true] 1
    [:false] 1
    [:zero] 1
    [:succ n] (inc (size n))
    [:pred n] (inc (size n))
    [:iszero n] (inc (size n))
    [:if c r1 r2] (+ (size c)
                     (size r1)
                     (size r2))))

(defn depth
  [e]
  (match e
    [:true] 1
    [:false] 1
    [:zero] 1
    [:succ n] (inc (depth n))
    [:pred n] (inc (depth n))
    [:iszero n] (inc (depth n))
    [:if c r1 r2] (inc (max (depth c)
                            (depth r1)
                            (depth r2)))))
