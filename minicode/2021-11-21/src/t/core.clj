(ns t.core
  (:require [clojure.core.match :refer [match]]))

(defn app
  [e1 e2]
  [:app e1 e2])

(defn exp-max
  [e]
  (match e
    [:var _] 0
    [:app e1 e2] (max (exp-max e1) (exp-max e2))
    [:abs n _] @n)) ;; this is the quadratic -> linear change

(defn abs
  [f]
  (let [n (promise)
        body (f [:var n])]
    (deliver n (inc (exp-max body)))
    [:abs n body]))

(defn printable-exp
  [exp]
  (match exp
    [:var p] [:var @p]
    [:app e1 e2] [:app (printable-exp e1) (printable-exp e2)]
    [:abs p b] [:abs @p (printable-exp b)]))

(comment

(abs (fn [m]
  (abs (fn [n]
    (abs (fn [f]
      (abs (fn [x]
        (app (app m f)
             (app (app n f)
                  x))))))))))
[:abs #object[clojure.core$promise$reify__8486 0x645c8466 {:status :ready, :val 4}]
 [:abs #object[clojure.core$promise$reify__8486 0x4b2f6ec9 {:status :ready, :val 3}]
  [:abs #object[clojure.core$promise$reify__8486 0x4b50ac7e {:status :ready, :val 2}]
   [:abs #object[clojure.core$promise$reify__8486 0x79d62380 {:status :ready, :val 1}]
    [:app [:app [:var #object[clojure.core$promise$reify__8486 0x645c8466 {:status :ready, :val 4}]]
           [:var #object[clojure.core$promise$reify__8486 0x4b50ac7e {:status :ready, :val 2}]]]
     [:app [:app [:var #object[clojure.core$promise$reify__8486 0x4b2f6ec9 {:status :ready, :val 3}]]
            [:var #object[clojure.core$promise$reify__8486 0x4b50ac7e {:status :ready, :val 2}]]]
      [:var #object[clojure.core$promise$reify__8486 0x79d62380 {:status :ready, :val 1}]]]]]]]]
)
