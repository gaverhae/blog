(ns t.untyped-lambda-test
  (:require [clojure.test :refer :all]
            [t.untyped-lambda :as t]))

(def id
  [:fn ["x"] [:var "x"]])

(deftest basic
  (are [x] (t/value? x)
       [:fn ["x"] [:app [:var "x"] [:var "x"]]]
       [:fn ["x"] :t])
  (are [x _ y] (= y (t/step x))
       [:var "x"] => nil
       [:app id id] => id
       [:app id [:app id id]] => [:app id id]
       [:app [:app id id] [:app id id]] => [:app id [:app id id]]
       [:app [:fn ["x"] [:var "x"]] id] => id
       [:app [:fn ["x"] [:var "y"]] id] => [:var "y"]
       [:app [:fn ["x"] id] [:fn ["y"] [:var "y"]]] => id
       [:app [:fn ["x"] [:app [:var "x"] [:var "x"]]] id] => [:app id id]

       [:app [:fn ["x"] [:fn ["y"] [:app [:var "x"] [:var "y"]]]] [:fn ["z"] [:var "z"]]]
       => [:fn ["y"] [:app [:fn ["z"] [:var "z"]] [:var "y"]]]
       ))
