(ns t.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [t.core :as t]
            [t.day1 :as day1]
            [t.day2 :as day2]))

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

(def sample5
"FBFBBFFRLR
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL")

(deftest d5
  (is (= [[44 5] [70 7] [14 7] [102 4]]
         (t/parse5 sample5))))
