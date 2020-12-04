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
