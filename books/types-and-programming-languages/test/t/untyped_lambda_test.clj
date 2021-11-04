(ns t.untyped-lambda-test
  (:require [clojure.test :refer :all]
            [t.untyped-lambda :as t]))

(deftest basic
  (are [x] (t/value? x)
       [:fn ["x"] [:app [:var "x"] [:var "x"]]])
  (are [x y] (= x (t/step y))
       nil [:var "x"]
       [:fn ["x"] [:var "x"]] [:app [:fn ["y"] [:var "y"]] [:fn ["x"] [:var "x"]]]))
