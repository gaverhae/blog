(ns t.arith.syntax
  (:refer-clojure :exclude [eval])
  (:require [t.lib :refer [match]]
            [instaparse.core :as insta]))

(def parser
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

(defn parse
  [s]
  (let [p (insta/parse parser s)]
    (if (insta/failure? p)
      (throw (Exception. "Parse failure."))
      (first p))))

(defn eval
  [e]
  (match e
    [:true] true
    [:false] false
    [:zero] 0
    [:succ n] (inc (eval n))
    [:pred n] (let [v (eval n)]
                (if (pos? v) (dec v) 0))
    [:iszero n] (zero? (eval n))
    [:if c r1 r2] (if (eval c)
                    (eval r1)
                    (eval r2))))

