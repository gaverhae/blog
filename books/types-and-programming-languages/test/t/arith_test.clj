(ns t.arith-test
  (:require [clojure.test :refer [deftest are]]
            [t.arith :as t]))

(deftest consts
  (are [x y] (= x (t/consts y))
       #{true} [:true]
       #{false} [:false]
       #{true 0} [:if [:true] [:succ [:zero]] [:pred [:zero]]]))

(deftest size
  (are [x y] (= x (t/size y))
       1 [:true]
       1 [:false]
       5 [:if [:true] [:succ [:zero]] [:pred [:zero]]]))

(deftest depth
  (are [x y] (= x (t/depth y))
       1 [:true]
       1 [:false]
       3 [:if [:true] [:succ [:zero]] [:pred [:zero]]]))
