(ns t.core-test
  (:require [clojure.test :refer :all]
            [t.core :as t]))

(deftest a-test
  (testing "generating a full deck"
    (is (= [1 2 3 4 5 6 7 8 9 10 11 12 13
            1 2 3 4 5 6 7 8 9 10 11 12 13
            1 2 3 4 5 6 7 8 9 10 11 12 13
            1 2 3 4 5 6 7 8 9 10 11 12 13]
           (t/deck))))
  (testing "count value of a hand"
    (is (= 14
           (t/hand-value [5 9])))
    (is (= 22
           (t/hand-value [1 1])))
    (is (= 34
           (t/hand-value [11 12 13 4])))
    (is (= [[2 7 2 6] [8]]
           (t/draw-until [2 7] 17 [2 6 8]))))
  (testing "end-to-end test"
    (are [x y] (= x (t/play-game y))
         ;; Sam wins by drawing 21 immediately
         :sam [1 10 2 2]
         ;; Dealer wins by drawing 21 immediately
         :dealer [2 2 1 10]
         ;; Sam wins if both have a 21 to start with
         :sam [1 10 1 10]
         ;; Sam loses by being over 21 form the start
         :dealer [1 1 3 3]
         ;; Sam loses by getting over 21 while drawing
         :dealer [3 3 3 3 10 1]
         ;; Sam wins immediately if they reach 21 while drawing
         :sam [3 3 3 3 10 5]
         ;; Sam reaches 18, dealer overshoots
         :sam [1 7 8 8 1]
         ;; Sam reaches 18, dealer gets to 19
         :dealer [1 7 8 8 2]

         ;; Game does not crash at end of deck
         :dealer [3 3 3 3 1 4])))
