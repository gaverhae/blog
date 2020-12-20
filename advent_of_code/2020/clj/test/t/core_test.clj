(ns t.core-test
  (:require [clojure.test :refer :all]; :exclude [is]]
            [clojure.string :as string]
            [t.core :as t]
            [t.day1 :as day1]
            [t.day2 :as day2]
            [t.day3 :as day3]
            [t.day4 :as day4]
            [t.day5 :as day5]
            [t.day6 :as day6]
            [t.day7 :as day7]
            [t.day8 :as day8]
            [t.day9 :as day9]
            [t.day10 :as day10]
            [t.day11 :as day11]
            [t.day12 :as day12]
            [t.day13 :as day13]
            [t.day14 :as day14]
            [t.day15 :as day15]
            [t.day16 :as day16]
            [t.day17 :as day17]
            [t.day18 :as day18]
            [t.day19 :as day19]))

(let [read (fn [s i] (string/split-lines (slurp (str "data/" s i))))]
  (defn sample [i] (read "sample" i))
  (defn data [i] (read "day" i)))


#_(defmacro is
  [form]
  `(do (prn (quote ~form)) (time (clojure.test/is ~form)) (println)))

(deftest day1
  (is (= [1721 979 366 299 675 1456]
         (day1/parse (sample 1))))
  (is (= [(* 1721 299)] (day1/part1 (day1/parse (sample 1)))))
  (is (= [712075] (day1/part1 (day1/parse (data 1)))))
  (is (= [145245270] (day1/part2 (day1/parse (data 1))))))

(deftest day2
  (is (= [[1 3 \a "abcde"] [1 3 \b "cdefg"] [2 9 \c "ccccccccc"]]
         (day2/parse (sample 2))))
  (is (= 2 (day2/part1 (day2/parse (sample 2)))))
  (is (= 666 (day2/part1 (day2/parse (data 2)))))
  (is (= 670 (day2/part2 (day2/parse (data 2))))))

(deftest day3
  (is (= [0 0 1 1 0 0 0 0 0 0 0 0 0 1 1]
         (->> (day3/parse (sample 3))
              first (take 15))))
  (is (= 7 (day3/part1 (day3/parse (sample 3)))))
  (is (= 214 (day3/part1 (day3/parse (data 3)))))
  (is (= 8336352024 (day3/part2 (day3/parse (data 3))))))

(deftest day4
  (is (= [{:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"}
          {:iyr "2013", :ecl "amb", :cid "350", :eyr "2023", :pid "028048884", :hcl "#cfa07d", :byr "1929"}
          {:hcl "#ae17e1", :iyr "2013", :eyr "2024", :ecl "brn", :pid "760753108", :byr "1931", :hgt "179cm"}
          {:hcl "#cfa07d", :eyr "2025", :pid "166559648", :iyr "2011", :ecl "brn", :hgt "59in"}]
         (day4/parse (sample 4))))
  (is (= 2 (day4/part1 (day4/parse (sample 4)))))
  (is (= 210 (day4/part1 (day4/parse (data 4)))))
  (is (= 131 (day4/part2 (day4/parse (data 4))))))

(deftest day5
  (is (= [357 567 119 820]
         (day5/parse (sample 5))))
  (is (= 820 (day5/part1 (day5/parse (sample 5)))))
  (is (= 826 (day5/part1 (day5/parse (data 5)))))
  (is (= 678 (day5/part2 (day5/parse (data 5))))))

(deftest day6
  (is (= [[#{\a \b \c}] [#{\a} #{\b} #{\c}] [#{\a \b} #{\a \c}] [#{\a} #{\a} #{\a} #{\a}] [#{\b}]]
         (day6/parse (sample 6))))
  (is (= 11 (day6/part1 (day6/parse (sample 6)))))
  (is (= 6443 (day6/part1 (day6/parse (data 6)))))
  (is (= 3232 (day6/part2 (day6/parse (data 6))))))

(deftest day7
  (is (= {"light red" {"bright white" 1, "muted yellow" 2}
          "dark orange" {"bright white" 3, "muted yellow" 4}
          "bright white" {"shiny gold" 1}
          "muted yellow" {"shiny gold" 2, "faded blue" 9}
          "shiny gold" {"dark olive" 1, "vibrant plum" 2}
          "dark olive" {"faded blue" 3, "dotted black" 4}
          "vibrant plum" {"faded blue" 5, "dotted black" 6}
          "faded blue" {}
          "dotted black" {}}
          (day7/parse (sample 7))))
  (is (= 4 (day7/part1 (day7/parse (sample 7)))))
  (is (= 128 (day7/part1 (day7/parse (data 7)))))
  (is (= 20189 (day7/part2 (day7/parse (data 7))))))

(deftest day8
  (is (= [[:nop 0]
          [:acc 1]
          [:jmp 4]
          [:acc 3]
          [:jmp -3]
          [:acc -99]
          [:acc 1]
          [:jmp -4]
          [:acc +6]]
         (day8/parse (sample 8))))
  (is (= [:loop 5] (day8/part1 (day8/parse (sample 8)))))
  (is (= [:loop 1709] (day8/part1 (day8/parse (data 8)))))
  (is (= [[:end 8]] (day8/part2 (day8/parse (sample 8)))))
  (is (= [[:end 1976]] (day8/part2 (day8/parse (data 8))))))

(deftest day9
  (is (= [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576]
         (day9/parse (sample 9))))
  (is (= 127 (day9/part1 5 (day9/parse (sample 9)))))
  (is (= 2089807806 (day9/part1 25 (day9/parse (data 9)))))
  (is (= 62 (day9/part2 5 (day9/parse (sample 9)))))
  (is (= 245848639 (day9/part2 25 (day9/parse (data 9))))))

(deftest day10
  (is (= [1 1 1 1 3 1 1 1 1 3 3 1 1 1 3 1 1 3 3 1 1 1 1 3 1 3 3 1 1 1 1 3]
         (day10/parse (sample 10))))
  (is (= 220 (day10/part1 (day10/parse (sample 10)))))
  (is (= 1700 (day10/part1 (day10/parse (data 10)))))
  (is (= 19208 (day10/part2 (day10/parse (sample 10)))))
  (is (= 12401793332096 (day10/part2 (day10/parse (data 10))))))

(deftest day11
  (is (= [10 10
          [1 0 1 1 0 1 1 0 1 1
           1 1 1 1 1 1 1 0 1 1
           1 0 1 0 1 0 0 1 0 0
           1 1 1 1 0 1 1 0 1 1
           1 0 1 1 0 1 1 0 1 1
           1 0 1 1 1 1 1 0 1 1
           0 0 1 0 1 0 0 0 0 0
           1 1 1 1 1 1 1 1 1 1
           1 0 1 1 1 1 1 1 0 1
           1 0 1 1 1 1 1 0 1 1]]
         (update (day11/parse (sample 11))
                 2 vec)))
  (is (= 37 (day11/part1 (day11/parse (sample 11)))))
  (is (= 2249 (day11/part1 (day11/parse (data 11)))))
  (is (= 26 (day11/part2 (day11/parse (sample 11)))))
  (is (= 2023 (day11/part2 (day11/parse (data 11))))))

(deftest day12
  (is (= [[:forward 10]
          [:move [0 3]]
          [:forward 7]
          [:turn 3]
          [:forward 11]]
         (day12/parse (sample 12))))
  (is (= 25 (day12/part1 (day12/parse (sample 12)))))
  (is (= 938 (day12/part1 (day12/parse (data 12)))))
  (is (= 286 (day12/part2 (day12/parse (sample 12)))))
  (is (= 54404 (day12/part2 (day12/parse (data 12))))))

(deftest day13
  (is (= [939
          [[:num 7] [:num 13] [:x] [:x] [:num 59] [:x] [:num 31] [:num 19]]]
         (day13/parse (sample 13))))
  (is (= 295 (day13/part1 (day13/parse (sample 13)))))
  (is (= 2305 (day13/part1 (day13/parse (data 13)))))
  (is (= 1068781 (day13/part2 (day13/parse (sample 13)))))
  (is (= 552612234243498 (day13/part2 (day13/parse (data 13))))))

(deftest day14
  (is (= [[:mask (->> (assoc (vec (take 36 (repeat :float)))
                             1 0
                             6 1)
                      (map-indexed vector)
                      (into {}))]
          [:mem 8 11]
          [:mem 7 101]
          [:mem 8 0]]
         (day14/parse (sample 14))))
  (is (= 165 (day14/part1 (day14/parse (sample 14)))))
  (is (= 9628746976360 (day14/part1 (day14/parse (data 14)))))
  (is (= 4574598714592 (day14/part2 (day14/parse (data 14))))))

(deftest day15
  (is (= [0 3 6]
         (day15/parse (sample 15))))
  (is (= 436 (day15/part1 (day15/parse (sample 15)))))
  (is (= 1111 (day15/part1 (day15/parse (data 15)))))
  (is (= 175594 (day15/part2 (day15/parse (sample 15)))))
  (is (= 48568 (day15/part2 (day15/parse (data 15))))))

(deftest day16
  (is (= {:rules {"class" [[1 3] [5 7]]
                  "row" [[6 11] [33 44]]
                  "seat" [[13 40] [45 50]]}
          :mine [7 1 14]
          :others [[7 3 47] [40 4 50] [55 2 20] [38 6 12]]}
         (day16/parse (sample 16))))
  (is (= 71 (day16/part1 (day16/parse (sample 16)))))
  (is (= 19240 (day16/part1 (day16/parse (data 16)))))
  (is (= 21095351239483 (day16/part2 (day16/parse (data 16))))))

(deftest day17
  (is (= #{[1 0 0] [2 1 0] [0 2 0] [1 2 0] [2 2 0]}
         (day17/parse (sample 17))))
  (is (= 112 (day17/part1 (day17/parse (sample 17)))))
  (is (= 380 (day17/part1 (day17/parse (data 17)))))
  (is (= 848 (day17/part2 (day17/parse (sample 17)))))
  (is (= 2332 (day17/part2 (day17/parse (data 17))))))

(deftest day18
  (is (= (+ 71 51 26 437 12240 13632)
         (day18/part1 (day18/parse (sample 18)))))
  (is (= 69490582260 (day18/part1 (day18/parse (data 18)))))
  (is (= (+ 231 51 46 1445 669060 23340)
         (day18/part2 (day18/parse (sample 18)))))
  (is (= 362464596624526 (day18/part2 (day18/parse (data 18))))))

(deftest day19
  (is (= {:grammar ["S: 0"
                    "0: 4 1 5"
                    "1: 2 3 | 3 2"
                    "2: 4 4 | 5 5"
                    "3: 4 5 | 5 4"
                    "4: \"a\""
                    "5: \"b\""]
          :lines ["ababbb"
                  "bababa"
                  "abbbab"
                  "aaabbb"
                  "aaaabbb"]}
         (day19/parse (sample 19))))
  (is (= 2
         (day19/part1 (day19/parse (sample 19)))))
  (is (= 269 (day19/part1 (day19/parse (data 19)))))
  (is (= 403 (day19/part2 (day19/parse (data 19))))))
