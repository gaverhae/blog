(ns t.untyped-lambda-test
  (:require [clojure.test :refer :all]
            [t.untyped-lambda :as t]))

(def id
  [:fn ["x"] [:var "x"]])

(deftest basic
  (are [x] (t/value? x)
       [:fn ["x"] [:app [:var "x"] [:var "x"]]]
       [:fn ["x"] :t])
  (are [x y] (= x (t/step y))
       nil [:var "x"]
       id [:app id id]
       [:app id id] [:app id [:app id id]]
       [:app id [:app id id]] [:app [:app id id] [:app id id]]
       id         [:app [:fn ["x"] [:var "x"]] id]
       [:var "y"] [:app [:fn ["x"] [:var "y"]] id]
       id [:app [:fn ["x"] id] [:fn ["y"] [:var "y"]]]
       [:app id id] [:app [:fn ["x"] [:app [:var "x"] [:var "x"]]] id]

       :undefined #_[:fn ["y"] [:app [:fn ["z"] [:var "z"]] [:var "y"]]]
       [:app [:fn ["x"] [:fn ["y"] [:app [:var "x"] [:var "y"]]]] [:fn ["z"] [:var "z"]]]
       ))
