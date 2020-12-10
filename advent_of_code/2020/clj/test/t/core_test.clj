(ns t.core-test
  (:require [clojure.test :refer :all]
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
            [t.day10 :as day10]))

(let [read (fn [s i] (string/split-lines (slurp (str "data/" s i))))]
  (defn sample [i] (read "sample" i))
  (defn data [i] (read "day" i)))

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
  (is (= [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35
          8 17 7 9 4 2 34 10 3]
         (day10/parse (sample 10))))
  (is (= 220 (day10/part1 (day10/parse (sample 10)))))
  (is (= 1700 (day10/part1 (day10/parse (data 10)))))
  (is (= 19208 (day10/part2 (day10/parse (sample 10)))))
  (is (= nil (day10/part2 (day10/parse (data 10))))))
