(ns t.core-test
  (:require [clojure.test :refer [deftest]]
            [clojure.string :as string]
            [t.day1 :as day1]
            [t.day2 :as day2]
            [t.day3 :as day3]))

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

(defmacro make-tests
  [& expected]
  `(do
     ~@(->> expected
            (map (fn [spec]
                   (let [d (str "day" (:day spec))
                         parse (symbol d "parse")
                         part1 (symbol d "part1")
                         part2 (symbol d "part2")
                         sample `(sample ~(:day spec))
                         data `(data ~(:day spec))
                         check (fn [expected actual]
                                 (when expected
                                   `(is (= ~expected ~actual))))]
                     `(deftest ~(symbol d)
                        ~(check (:sample spec)
                                `(~parse ~sample))
                        ~(check (get-in spec [:part1 0])
                                `(~part1 (~parse ~sample)))
                        ~(check (get-in spec [:part1 1])
                                `(~part1 (~parse ~data)))
                        ~(check (get-in spec [:part2 0])
                                `(~part2 (~parse ~sample)))
                        ~(check (get-in spec [:part2 1])
                                `(~part2 (~parse ~data))))))))))

(make-tests

  {:day 1
   :sample [199 200 208 210 200 207 240 269 260 263]
   :part1 [7 1292]
   :part2 [5 1262]}

  {:day 2
   :sample [[:forward 5] [:down 5] [:forward 8] [:up 3] [:down 8] [:forward 2]]
   :part1 [150 1660158]
   :part2 [900 1604592846]}

  {:day 3
   :sample ["00100" "11110" "10110" "10111" "10101" "01111"
            "00111" "11100" "10000" "11001" "00010" "01010"]
   :part1 [198 4103154]
   :part2 [230 4245351]})
