(ns t.arith-test
  (:require [clojure.test :refer :all]
            [t.arith :as t]))

(deftest arith
  (are [v] (t/value? v)
       :true
       :false
       :0
       [:succ [:succ [:succ :0]]])
  (are [v] (t/normal? v)
       :true
       :false
       :0
       [:succ [:succ :0]]
       [:succ [:succ [:pred :true]]]
       [:if :0 :true :false]
       [:pred :false])
  (are [out in] (= out (t/step in))
       :true [:if :true :true :false]
       :false [:if :false :true :false]
       [:if :true :true :false] [:if :true [:if :true :true :false] :false]
       [:if :true :true :false] [:if [:if :false :false :true] :true :false]
       :0 [:pred :0]
       :0 [:pred [:succ :0]])
  (are [out in] (= out (t/eval in))
       [:value :true] [:if :true :true :false]
       [:value :false] [:if :false :true :false]
       [:value :true] [:if :true [:if :true :true :false] :false]
       [:value :true] [:if [:if :false :false :true] :true :false]
       [:value :0] [:pred [:succ [:pred :0]]]
       [:value :true] [:if [:zero? [:pred [:succ [:pred :0]]]] :true :false]
       [:stuck [:if :0 :true :false]] [:if :0 :true :false]
       [:stuck [:pred [:succ [:pred [:succ [:succ :true]]]]]]
       [:if [:zero? [:pred [:succ [:pred :0]]]]
          [:pred [:succ [:pred [:succ [:succ :true]]]]]
          :0]))
