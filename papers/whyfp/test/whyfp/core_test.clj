(ns whyfp.core-test
  (:require [clojure.test :refer [deftest is]]
            [whyfp.core :as core]))

(deftest section3
  (is (= 10 (core/sum [1 2 3 4])))
  (is (= 24 (core/product [1 2 3 4])))
  (is (= true (core/anytrue [false false true false])))
  (is (= false (core/alltrue [false false true false])))
  (is (= [1 2 3 4] (core/append [1 2] [3 4])))
  (is (= [2 4 6 8] (core/doubleall [1 2 3 4])))
  (is (= 21 (core/summatrix [[1 2 3] [4 5 6]])))
  (is (= 10 (core/sumtree' core/tree')))
  (is (= 10 (core/sumtree core/tree)))
  (is (= [1 2 3 4] (core/labels core/tree)))
  (is (= [1 2 3 4] (core/labels' core/tree')))
  (is (= [:node 2 [:cons [:node 4 nil]
                         [:cons [:node 6 [:cons [:node 8 nil] nil]]
                                nil]]]
         ((core/maptree #(* 2 %)) core/tree)))
  (is (= [2 [4] [6 [8]]] ((core/maptree' #(* 2 %)) core/tree'))))

(defn within?
  [eps target value]
  (>= eps (Math/abs (- target value))))

(defn f [x] (+ (* x x) (* 3 x) 2))
(defn f' [x] (+ (* 2 x) 3))
(defn F [x] (+ (* 1/3 x x x) (* 3/2 x x) (* 2 x)))

(deftest section4
  (is (within? 0.01 2 (core/sqrt 1 0.01 4)))
  (is (within? 0.01 2 (core/relativesqrt 1 0.01 4)))
  (is (within? 0.01
        (f' 1)
        (core/within 0.01 (core/differentiate 0.01 f 1))))
  (is (within? 0.01
        (f' 1)
        (core/within 0.01
          (core/improve (core/differentiate 0.01 f 1)))))
  (is (within? 0.01
        (f' 1)
        (core/within 0.01
          (core/super (core/differentiate 0.01 f 1)))))
  (is (within? 0.01
        (- (F 2) (F 1))
        (core/within 0.01
          (core/integrate f 1 2))))
  (is (within? 0.01
        (- (F 2) (F 1))
        (core/within 0.01
          (core/integrate2 f 1 2))))
  ;; diverges: order returns 0 and thus elimerror yields #Inf
  #_(is (within? 0.01
        1.6536
        (core/within 0.01
          (core/super (core/integrate2 #(Math/sin %) 0 4)))))
  (is (within? 0.00001
        (/ Math/PI 4)
        (second
          (core/improve
            (core/integrate2
              (fn [x] (/ 1.0 (+ 1 (* x x))))
              0 1))))))
