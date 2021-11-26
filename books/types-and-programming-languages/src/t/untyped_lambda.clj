(ns t.untyped-lambda
  (:refer-clojure :exclude [test eval and])
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]))

(defn value?
  [exp]
  (match exp
    [:fn v t] true
    _ false))

(defn free-variables
  [exp]
  (match exp
    [:var x] #{x}
    [:app t1 t2] (set/union (free-variables t1)
                            (free-variables t2))
    [:fn x t] (disj (free-variables t)
                    x)))

(defn generate-name
  [free-vars]
  (->> (range)
       (map #(Integer/toString % 36))
       (remove (set free-vars))
       first))

(defn substitute
  [var-name with in]
  (match in
    [:var var-name] with
    [:var other] in
    [:fn var-name t] in
    [:fn other t] (let [fv (free-variables with)]
                    (if (contains? fv other)
                      (let [new-name (generate-name fv)]
                        [:fn new-name (->> t
                                           (substitute other [:var new-name])
                                           (substitute var-name with))])
                      [:fn other (substitute var-name with t)]))
    [:app t1 t2] [:app (substitute var-name with t1)
                       (substitute var-name with t2)]))

(defn step
  [exp]
  (match exp
    [:app [:fn x t] (v :guard value?)] (substitute x v t)
    [:app (v :guard value?) t] (when-let [t' (step t)]
                                 [:app v t'])
    [:app t1 t2] (when-let [t1' (step t1)]
                   [:app t1' t2])
    _ nil))

(defn normal?
  [exp]
  (nil? (step exp)))

(defn eval
  [exp]
  (if-let [next (step exp)]
    (eval next)
    [(if (value? exp) :value :stuck) exp]))

(def tru [:fn "t" [:fn "f" [:var "t"]]])
(def fls [:fn "t" [:fn "f" [:var "f"]]])
(def test [:fn "l" [:fn "m" [:fn "n" [:app [:app [:var "l"] [:var "m"]] [:var "n"]]]]])
(def and [:fn "b" [:fn "c" [:app [:app [:var "b"] [:var "c"]] fls]]])
