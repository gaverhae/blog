;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;;
;;; Welcome to gorilla :-)
;;;
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;;
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns juicy-moss
  (:require [gorilla-plot.core :as plot]
            [gorilla-repl.table :as table]
            [gorilla-renderable.core :as render]))
;; @@

;; @@
(defn roll
  "Rolls `dice` dice with `sides` sides each and sums the results."
  [sides dice]
  ; (rand-int n) is within [0, n-1] so we add `dice`
  (reduce + dice (repeatedly dice #(rand-int sides))))
;; @@

;; @@
(repeatedly 10 #(roll 6 2))
;; @@

;; @@
(defn reup-roll
  "Rolls `n` d6 dice according to SW:Reup rules, i.e. with a 'wild' die."
  [n]
  (let [n (dec n)
        normals (map inc (repeatedly n #(rand-int 6)))
        sum-norm (reduce + 0 normals)
        max-norm (reduce max 0 normals)
        wild (loop [v 0]
               (let [d (inc (rand-int 6))]
                 (if (== 6 d)
                   (recur (+ v d))
                   (+ v d))))]
    (if (== 1 wild)
      (- sum-norm max-norm)
      (+ wild sum-norm))))

;; @@

;; @@
(repeatedly 10 #(reup-roll 1))
;; @@

;; @@
(defn plot
  [f]
  (let [sample (repeatedly (* 1000 1000) f)
        freqs (frequencies sample)
        values (-> freqs keys sort)
        occurrences (->> freqs sort (map val))]
    occurrences
    (plot/bar-chart values occurrences)))
;; @@

;; @@
(->> (range 1 7) (map (fn [i] [(plot #(roll 6 i)) (plot #(reup-roll i))])))
;; @@

;; @@
(defn quantiles
  [f]
  (let [n (* 1000 1000)
        values (->> (repeatedly n f) sort vec)
        avg (/ (reduce + 0 values) 1.0 n)
        step (/ n 20)]
    [avg
     (get values (dec step))
     (get values (dec (* 5 step)))
     (get values (dec (* 10 step)))
     (get values (dec (* 15 step)))
     (get values (dec (* 19 step)))]))
;; @@

;; @@
(def space (reify render/Renderable (render [_] {:type :html :content "" :value ""})))
(defn raw-string [s] (reify render/Renderable (render [_] {:type :html :content s :value s})))

(table/table-view (->>
                    (range 1 7)
                    (mapcat (fn [i]
                              [(cons (raw-string (str i "d6")) (quantiles #(roll 6 i)))
                               (cons (raw-string (str i "d6 wild")) (quantiles #(reup-roll i)))
                               (repeat 7 space)]))
                    (cons (repeat 7 space))
                    (cons (map raw-string ["" "avg" "5%" "25%" "50%" "75%" "95%"]))))
;; @@
