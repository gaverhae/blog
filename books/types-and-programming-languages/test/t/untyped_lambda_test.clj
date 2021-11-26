(ns t.untyped-lambda-test
  (:require [clojure.test :refer :all]
            [t.untyped-lambda :as t]))

(def id
  [:fn "x" [:var "x"]])

(def v [:fn "v" [:var "v"]])
(def w [:fn "w" [:var "w"]])

(deftest basic
  (are [x] (t/value? x)
       [:fn "x" [:app [:var "x"] [:var "x"]]]
       [:fn "x" :t])
  (are [x _ y] (= y (t/step x))
       [:var "x"] => nil
       [:app id id] => id
       [:app id [:app id id]] => [:app id id]
       [:app [:app id id] [:app id id]] => [:app id [:app id id]]
       [:app [:fn "x" [:var "x"]] id] => id
       [:app [:fn "x" [:var "y"]] id] => [:var "y"]
       [:app [:fn "x" id] [:fn "y" [:var "y"]]] => id
       [:app [:fn "x" [:app [:var "x"] [:var "x"]]] id] => [:app id id]

       [:app [:fn "x" [:fn "y" [:app [:var "x"] [:var "y"]]]] [:fn "z" [:var "z"]]]
       => [:fn "y" [:app [:fn "z" [:var "z"]] [:var "y"]]]

       [:app [:fn "x" [:fn "y" [:app [:var "x"] [:var "y"]]]] [:fn "z" [:app [:var "z"] [:var "t"]]]]
       => [:fn "y" [:app [:fn "z" [:app [:var "z"] [:var "t"]]] [:var "y"]]]

       [:app [:fn "x" [:fn "y" [:app [:var "x"] [:var "y"]]]] [:fn "z" [:app [:var "z"] [:var "y"]]]]
       => [:fn "0" [:app [:fn "z" [:app [:var "z"] [:var "y"]]] [:var "0"]]])
  (are [x _ y] (= y (->> x (iterate t/step) rest (take (count y))))
       [:app [:app [:app t/test t/tru] v] w]
       => [[:app [:app [:fn "m" [:fn "n" [:app [:app t/tru [:var "m"]] [:var "n"]]]] v] w]
           [:app [:fn "n" [:app [:app t/tru v] [:var "n"]]] w]
           [:app [:app t/tru v] w]
           [:app [:fn "f" v] w]
           v])
  (are [x _ y] (= y (t/eval x))
       [:app [:app [:app t/test t/tru] v] w] => [:value v]
       [:app [:app t/and t/tru] t/tru] => t/tru
       ))
