(ns t.core-test
  (:require [clojure.test :refer [deftest]]
            [clojure.string :as string]
            [t.day1 :as day1]
            [t.day2 :as day2]))

(let [read (fn [s i] (string/split-lines (slurp (str "data/" s i))))]
  (defn sample [i] (read "sample" i))
  (defn data [i] (read "day" i)))

(defmacro is
  [form]
  `(let [form# (quote ~form)
         start# (System/currentTimeMillis)
         _# (clojure.test/is ~form)
         t# (- (System/currentTimeMillis) start#)]
     (when (> t# 100)
       (println (format "%d %s" t# (nth form# 2))))))

(deftest day1
  (is (= [199 200 208 210 200 207 240 269 260 263]
         (day1/parse (sample 1))))
  (is (= 7 (day1/part1 (day1/parse (sample 1)))))
  (is (= 1292 (day1/part1 (day1/parse (data 1)))))
  (is (= 1262 (day1/part2 (day1/parse (data 1))))))

(deftest day2
  (is (= [[5 0] [0 5] [8 0] [0 -3] [0 8] [2 0]]
         (day2/parse (sample 2))))
  (is (= 150 (day2/part1 (day2/parse (sample 2)))))
  (is (= 1660158 (day2/part1 (day2/parse (data 2)))))
  (is (= 900 (day2/part2 (day2/parse (sample 2)))))
  (is (= 1604592846 (day2/part2 (day2/parse (data 2))))))
