(ns whyfp.core
  (:refer-clojure :exclude [double map next repeat])
  (:require [clojure.core.match :refer [match]]))

;; Section 3

(comment
  ; sum Nil = 0
  ; sum (Cons n list) = n + sum list
  (defn sum
    [ls]
    (match ls
      ([] :seq) 0
      ([hd & tl] :seq) (+ hd (sum tl))))
  )

; listof * ::= Nil | Cons * (listof *)
;; Nil -> []
;; Cons 1 (Cons 2 (Cons 3 Nil)) -> [1 2 3]

; (foldr f x) Nil = x
; (foldr f x) (Cons a l) = f a ((foldr f x) l)
(defn foldr
  [f init]
  (fn rec [ls]
    (match ls
      ([] :seq) init
      ([hd & tl] :seq) (f hd (rec tl)))))

; sum = foldr (+) 0
(def sum (foldr + 0))

; product = foldr (*) 1
(def product (foldr * 1))

; anytrue = foldr (|) False
(def anytrue (foldr #(or %1 %2) false))

; alltrue = foldr (&) True
(def alltrue (foldr #(and %1 %2) true))

; append a b = foldr Cons b a
(defn append [a b] ((foldr cons b) a))

; length = foldr count 0
; count a n = n + 1
(def length (foldr #(inc %2) 0))

; doubleall = map double
; double n = 2 * n
; map f = foldr (Cons . f) Nil -- currying doesn't render well in Clojure
(defn double [n] (* 2 n))
(defn map [f] (foldr (fn [hd tl] (cons (f hd) tl))
                     nil))
(def doubleall (map double))

; summatrix = sum . map sum
(def summatrix (comp sum (map sum)))

; treeof * ::= Node * (listof (treeof *))
;; Node 1 (Cons (Node 2 Nil)
;;              (Cons (Node 3 (Cons (Node 4 Nil) Nil))
;;                    Nil))
(def tree'
  [1 [2] [3 [4]]])
;; or
(def tree
  [:node 1 [:cons [:node 2 nil]
                  [:cons [:node 3 [:cons [:node 4 nil] nil]]
                         nil]]])

; foldtree f g a (Node label subtrees) =
;   f label (foldtree f g a subtrees)
; foldtree f g a (Cons subtree rest) =
;   g (foldtree f g a subtree) (foldtree f g a rest)
; foldtree f g a Nil = a
(defn foldtree'
  [node-fn cons-fn nil-value]
  (fn rec [tree]
    (match tree
      ([] :seq) nil-value
      ([value & subtrees] :seq) (node-fn value
                                         ((foldr cons-fn nil-value) ((map rec) subtrees))))))
; sumtree = foldtree (+) (+) 0
(def sumtree' (foldtree' + + 0))
; labels = foldtree Cons append Nil
(def labels' (foldtree' cons append nil))
; maptree f = foldtree (Node . f) Cons Nil
(defn maptree'
  [f]
  (foldtree' (fn [v s] (cons (f v) s)) cons '()))

(defn foldtree
  [node-fn cons-fn nil-value]
  (fn rec [tree]
    (match tree
      [:node v subtrees] (node-fn v (rec subtrees))
      [:cons node subtrees] (cons-fn (rec node) (rec subtrees))
      nil nil-value)))
; sumtree = foldtree (+) (+) 0
(def sumtree (foldtree + + 0))
; labels = foldtree Cons append Nil
(def labels (foldtree cons append nil))
; maptree f = foldtree (Node . f) Cons Nil
(defn maptree
  [f]
  (foldtree (fn [v s] [:node (f v) s])
            (fn [hd tl] [:cons hd tl])
            nil))
