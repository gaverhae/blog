(ns whyfp.core
  (:refer-clojure :exclude [double map next repeat])
  (:require [clojure.core.match :refer [match]]))

(def map' clojure.core/map)

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
(def tree
  [1 [[2 []] [3 [[4 []]]]]])

; foldtree f g a (Node label subtrees) =
;   f label (foldtree f g a subtrees)
; foldtree f g a (Cons subtree rest) =
;   g (foldtree f g a subtree) (foldtree f g a rest)
; foldtree f g a Nil = a
(defn foldtree
  [node-fn list-fn zero [node children]]
  ;; This is a foldr, so it's not lazy
  (node-fn node
           (reduce list-fn
                   zero
                   (map' #(foldtree node-fn list-fn zero %) children))))

; sumtree = foldtree (+) (+) 0
(def sumtree #(foldtree + + 0 %))
; labels = foldtree Cons append Nil
(def labels #(foldtree cons append nil %))
; maptree f = foldtree (Node . f) Cons Nil
(defn maptree
  [f]
  #(foldtree
;; (Node . f) doesn't translate
     (fn [node children] [(f node) children])
     conj [] %))

;; Section 4

; next n x = (x + n/x) / 2
(defn next [n x] (/ (+ x (/ n 1.0 x)) 2.0))
; repeat f a = Cons a (repeat f (f a))
(defn repeat [f a] (cons a (lazy-seq (repeat f (f a)))))
; within eps (Cons a (Cons b rest))
;   = b, if abs (a - b) <= eps
;   = with eps (Cons b rest), otherwise
(defn within
  [eps [a b & rs]]
  (if (>= eps (Math/abs (- a b)))
    b
    (within eps (cons b rs))))
; sqrt a0 eps n = within eps (repeat (next n) a0)
(defn sqrt
  [a0 eps n]
  (within eps (repeat #(next n %) a0)))

; relative eps (Cons a (Cons b rest))
;   = b, if abs (a/b - 1) <= eps
;   = relative eps (Cons b rest), otherwise
(defn relative
  [eps [a b & rs]]
  (if (>= eps (Math/abs (dec (/ a 1.0 b))))
    b
    (relative eps (cons b rs))))

; relativesqrt a0 eps n = relative eps (repeat (next n) a0)
(defn relativesqrt
  [a0 eps n]
  (relative eps (repeat #(next n %) a0)))

; easydiff f x h = (f(x + h) - f x) / h
(defn easydiff
  [f x h]
  (/ (- (f (+ x h))
        (f x))
     h))

; differentiate h0 f x = map (easydiff f x) (repeat halve h0)
; halve x = x / 2

;; Note: foldr is not lazy, so map is not lazy
(defn halve [x] (/ x 2.0))
(defn differentiate
  [h0 f x]
  (map' (fn [h] (easydiff f x h)) (repeat halve h0)))

; elimerror n (Cons a (Cons b rest))
; = Cons ((b * (2^n) - a)/(2^n-1)) (elimerror n (Cons b rest))
(defn elimerror
  [n [a b & tl]]
  (cons (/ (- (* b (Math/pow 2.0 n))
              a)
           (- (Math/pow 2.0 n) 1.0))
        (lazy-seq (elimerror n (cons b tl)))))

; order (Cons a (Cons b (Cons c rest)))
; = round (log2 ((a - c)/(b - c) -1))
; round x = x rounded to the nearest integer
; log2 x = the logarithm of x to the base 2
(defn round [x] (Math/round x))
(defn log2 [x] (/ (Math/log x) (Math/log 2.0)))
(defn order
  [[a b c & tl]]
  (round (log2 (- (/ (- a c)
                     (- b c))
                  1.0))))

; improve s = elimerror (order s) s
(defn improve
  [s]
  (elimerror (order s) s))

; super s = map second (repeat improve s)
; second (Cons a (Cons b rest)) = b
(defn super
  [s]
  (map' second (repeat improve s)))

; easyintegrate f a b = (f a + f b) * (b - a) / 2
(defn easyintegrate
  [f a b]
  (* (+ (f a) (f b))
     (- b a)
     0.5))

; integrate f a b = Cons (easyintegrate f a b)
;                        (map addpair (zip2 (integrate f a mid)
;                                           (integrate f mid b)))
; where mid = (a + b) / 2
; zip2 (Cons a s) (Cons b t) = Cons (a, b) (zip2 s t)
;; no base case for zip2
;; addpair is never defined
(defn integrate
  [f a b]
  (let [mid (/ (+ a b) 2.0)]
    (cons (easyintegrate f a b)
          ;; example of indentation no autoindent can help with
          (lazy-seq (map' + (integrate f a mid)
                            (integrate f mid b))))))

; integrate f a b = integ f a b (f a) (f b)
; integ f a b fa fb = Cons ((fa + fb) * (b - &) / 2)
;                          map addpair (zip2 (integ f a m fa fm)
;                                            (integ f m b fm fb))) ;; yes, that's an unbalanced paren
; where m = (a + b) / 2
;       fm = f m
(defn integrate2
  [f a b]
  (let [integ (fn integ [f a b fa fb]
                (let [m (/ (+ a b) 2.0)
                      fm (f m)]
                  (cons (* (+ fa fb) (- b a) 0.5)
                        (lazy-seq (map' + (integ f a m fa fm)
                                          (integ f m b fm fb))))))]
    (integ f a b (f a) (f b))))

;; Section 5

;; left as an exercise to the reader
; moves :: position -> listof position
(def start-pos-tic-tac-toe
  {:next-player 1 :board [[0 0 0] [0 0 0] [0 0 0]]})

(defn winner-tic-tac-toe
  [{:keys [board]}]
  (let [horizontals (for [y (range 3)]
                      (for [x (range 3)]
                        [y x]))
        verticals (for [x (range 3)]
                    (for [y (range 3)]
                      [y x]))
        diagonals [[[0 0] [1 1] [2 2]]
                   [[0 2] [1 1] [2 0]]]
        lines (concat horizontals verticals diagonals)]
    (->> lines
         (map' (fn [line]
                 (->> line
                      (map' (fn [pos] (get-in board pos)))
                      set)))
         (filter (fn [s] (= 1 (count s))))
         (map' first)
         (remove #{0})
         first)))

(defn moves-tic-tac-toe
  [{:keys [board next-player] :as state}]
  (if (winner-tic-tac-toe state)
    []
    (for [y (range 3)
          x (range 3)
          :let [p (get-in board [y x])]
          :when (zero? p)]
      {:board (assoc-in board [y x] next-player)
       :next-player ({1 2, 2 1} next-player)})))


; reptree f a = Node a (map (reptree f) (f a))
; gametree p = reptree moves p
(defn reptree
  [f a]
  {:value a
   :children (map' (fn [p] (reptree f p)) (f a))})

;; also left as an exercise
; static :: position -> number
(defn static-tic-tac-toe
  "1 if player 1 wins, -1 if player 2 wins, 0 otherwise. Assumes game has not
  been played past first win condition."
  [state]
  (case (winner-tic-tac-toe state)
    nil 0
    1 1
    2 -1))

;; maptree above uses foldr and thus is eager; let's make a lazy one
(defn maptree'
  [f {:keys [value children]}]
  {:value (f value)
   :children (map' #(maptree' f %) children)})

; maximize (Node n sub) = max (map minimize sub)
; maximize (Node n Nil) = n
; minimize (Node n sub) = min (map maximize sub)
; minimize (Node n Nil) = n
;; Clojure compiler is single-passe
(declare minimize)
(defn maximize
  [{:keys [value children]}]
  (if (empty? children)
    value
    ;; Clojure builtin max is not built on lists
    (reduce max ((map minimize) children))))
(defn minimize
  [{:keys [value children]}]
  (if (empty? children)
    value
    (reduce min ((map maximize) children))))

; prune 0 (Node a x) = Node a Nil
; prune (n+1) (Node a x) = Node a (map (prune n) x)
(defn prune
  [n {:keys [value children]}]
  {:value value
   :children (if (zero? n)
               []
               (map' #(prune (dec n) %) children))})

; evaluate = maximize . maptree static . prune 5 . gametree
(defn evaluate
  [lookahead moves static pos]
  (->> pos
       (reptree moves)
       (prune lookahead)
       (maptree' static)
       maximize))

; no more useful points in the rest, but lots of badly-named, badly-factored,
; hard-to-understand functions.
