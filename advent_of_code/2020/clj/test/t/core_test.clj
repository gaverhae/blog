(ns t.core-test
  (:require [clojure.test :refer :all]
            [t.core :as t]))

(deftest day-1-part-1
  (is (= [1721 299]
         (t/sum-2020 [1721
                      979
                      366
                      299
                      675
                      1456])))
  (is (= 712075 (t/day-1-part-1))))

(deftest day-2
  (is (= [[1 3 \a "abcde"]
          [1 3 \b "cdefg"]
          [2 9 \c "ccccccccc"]]
         (t/parse-day-2 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc")))
  (is (= 2
         (t/count-good-passwords [[1 3 \a "abcde"]
                                  [1 3 \b "cdefg"]
                                  [2 9 \c "ccccccccc"]])))
  (is (= 1
         (t/count-good-passwords-2 [[1 3 \a "abcde"]
                                    [1 3 \b "cdefg"]
                                    [2 9 \c "ccccccccc"]]))))

(def day-3-sample
  "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#")

(deftest day-3
  (is (= [0 0 1 1 0 0 0 0 0 0 0 0 0 1 1] (->> (t/day-3-parse day-3-sample)
                                              first
                                              (take 15))))
  (is (= 7 (t/slope [3 1] (t/day-3-parse day-3-sample))))
  (is (= 336 (t/day-3-part-2 day-3-sample))))

(def sample4
"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(deftest day-4
  (is (= [{:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"}
          {:iyr "2013", :ecl "amb", :cid "350", :eyr "2023", :pid "028048884", :hcl "#cfa07d", :byr "1929"}
          {:hcl "#ae17e1", :iyr "2013", :eyr "2024", :ecl "brn", :pid "760753108", :byr "1931", :hgt "179cm"}
          {:hcl "#cfa07d", :eyr "2025", :pid "166559648", :iyr "2011", :ecl "brn", :hgt "59in"}]
         (t/parse4 sample4)))
  (is (= 2 (t/d41 (t/parse4 sample4)))))
