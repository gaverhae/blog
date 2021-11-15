(ns t.untyped-lambda
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]))

(defn value?
  [exp]
  (match exp
    [:fn [v] t] true
    _ false))

(defn free-variables
  [exp]
  (match exp
    [:var x] #{x}
    [:app t1 t2] (set/union (free-variables t1)
                            (free-variables t2))
    [:fn [x] t] (disj (free-variables t)
                      x)))

(defn avoid-collisions
  [exp n]
  (let [fv (free-variables exp)]
    (if (fv n)
      :undefined
      exp)))

(defn substitute
  [var-name with in]
  (match in
    [:var var-name] with
    [:var other] in
    [:fn [var-name] t] in
    [:fn [other] t] [:fn [other] (substitute var-name
                                             (avoid-collisions with other)
                                             t)]
    [:app t1 t2] [:app (substitute var-name with t1)
                       (substitute var-name with t2)]))

(defn step
  [exp]
  (match exp
    [:app [:fn [x] t] (v :guard value?)] (substitute x v t)
    [:app (v :guard value?) t] (when-let [t' (step t)]
                                 [:app v t'])
    [:app t1 t2] (when-let [t1' (step t1)]
                   [:app t1' t2])
    _ nil))

(defn normal?
  [exp]
  (nil? (step exp)))
