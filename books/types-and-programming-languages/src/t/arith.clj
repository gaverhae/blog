(ns t.arith
  (:refer-clojure :exclude [eval])
  (:require [clojure.core.match :refer [match]]))

(defn numeric?
  [exp]
  (match exp
    :0 true
    [:succ n] (numeric? n)
    _ false))

(defn value?
  [exp]
  (match exp
    :true true
    :false true
    v (numeric? v)))

(defn step
  [exp]
  (match exp
    [:if :true t2 t3] t2
    [:if :false t2 t3] t3
    [:if t1 t2 t3] (when-let [t1' (step t1)]
                     [:if t1' t2 t3])
    [:succ t1] (when-let [t1' (step t1)]
                 [:succ t1'])
    [:pred :0] :0
    [:pred [:succ (nv :guard numeric?)]] nv
    [:pred t1] (when-let [t1' (step t1)]
                 [:pred t1'])
    [:zero? :0] :true
    [:zero? [:succ (nv :guard numeric?)]] :false
    [:zero? t1] (when-let [t1' (step t1)]
                  [:zero? t1'])
    else nil))

(defn normal?
  [exp]
  (nil? (step exp)))

(defn eval
  [exp]
  (if-let [next (step exp)]
    (eval next)
    [(if (value? exp) :value :stuck) exp]))

(defn big-eval
  [exp]
  (match exp
    (v :guard value?) [:value v]
    [:if t1 t2 t3] (match (big-eval t1)
                     [:value :true] (big-eval t2)
                     [:value :false] (big-eval t3)
                     else [:stuck exp])
    [:succ t1] (match (big-eval t1)
                 [:value (nv :guard numeric?)] [:value [:succ nv]]
                 else [:stuck exp])
    [:pred t1] (match (big-eval t1)
                 [:value :0] [:value :0]
                 [:value [:succ (nv :guard numeric?)]] [:value nv]
                 else [:stuck exp])
    [:zero? t1] (match (big-eval t1)
                   [:value :0] [:value :true]
                   [:value [:succ (nv :guard numeric?)]] [:value :false]
                   [:value v] [:stuck [:zero? v]]
                   else [:stuck exp])
    else [:stuck exp]))
