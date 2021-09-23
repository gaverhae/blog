(ns t.core
  (:require [instaparse.core :as insta]))

(defmacro match
  [expr & cases]
  (let [e (gensym)]
    `(let [~e ~expr]
       (case (first ~e)
         ~@(->> (partition 2 cases)
                (mapcat (fn [[pat body]]
                          [(first pat) `(let [~(vec (cons '_ (rest pat))) ~e]
                                          ~body)])))))))

(def arith-syntax
  (insta/parser
    "<S> := <w> t <w>
     <t> := terminal | composed
     <terminal> := true
                 | false
                 | zero
     <composed> := if
                 | succ
                 | pred
                 | iszero
     true := <'true'>
     false := <'false'>
     if := <'if'> pt <'then'> pt <'else'> pt
     zero := <'0'>
     succ := <'succ'> pt
     pred := <'pred'> pt
     iszero := <'iszero'> pt
     <pt> := <w>
             (       <w> terminal <w>
             | <'('> <w> terminal <w> <')'>
             | <'('> <w> composed <w> <')'>
             )
             <w>
     w := #'\\s*'
    "))

(defn parse-arith
  [s]
  (let [p (insta/parse arith-syntax s)]
    (if (insta/failure? p)
      (throw (Exception. "Parse failure."))
      (first p))))

(defn eval-arith
  [e]
  (match e
    [:true] true
    [:false] false
    [:if c r1 r2] (if (eval-arith c)
                    (eval-arith r1)
                    (eval-arith r2))
    [:zero] 0
    [:succ n] (inc (eval-arith n))
    [:pred n] (dec (eval-arith n))
    [:iszero n] (zero? (eval-arith n))))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
