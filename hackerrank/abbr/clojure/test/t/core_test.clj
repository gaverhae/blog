(ns t.core-test
  (:require [clojure.test :refer :all]
            [t.core :refer :all]))

(deftest a-test
  (is (abbr "daBcd" "ABC"))
  (is (not (abbr "AbCdE" "AFE")))
  (is (not (abbr "beFgH" "EFG")))
  (is (abbr "beFgH" "EFH")))
