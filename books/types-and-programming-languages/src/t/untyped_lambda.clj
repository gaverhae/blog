(ns t.untyped-lambda
  (:require [clojure.core.match :refer [match]]))

(defn value?
  [exp]
  (match exp
    [:fn [v] t] true
    _ false))

(defn substitute
  [var-name with in]
  (match in
    [:var var-name] with
    [:var other] in
    [:fn [var-name] t] nil ;; TODO
    [:fn [other] t] nil ;; TODO
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
