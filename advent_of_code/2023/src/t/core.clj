(ns t.core
  (:require [clojure.string :as s]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(-> (slurp "day1.txt")
    (s/split #"\n")
    (->> (map (fn [s] (let [x (filter #(Character/isDigit %) s)]
                        (Long/parseLong (str (first x) (last x))))))
         (reduce + 0)))
54644

(-> (slurp "sample1.txt")
    (s/split #"\n")
    (->> (map (fn [s]
                (let [x (re-seq #"one|two|three|four|five|six|seven|eight|nine|[0-9]" s)
                      x (->> x (map #(get {"one" "1"
                                           "two" "2"
                                           "three" "3"
                                           "four" "4"
                                           "five" "5"
                                           "six" "6"
                                           "seven" "7"
                                           "eight" 8
                                           "nine" "9"} % %)))]
                  (Long/parseLong (str (first x) (last x))))))
         (reduce + 0)))
281

(-> (slurp "sample1.txt")
    (s/split #"\n")
    (->> (take 5)
         (map (fn [s]
                (let [x (re-seq #"one|two|three|four|five|six|seven|eight|nine|[0-9]" s)
                      #_#_x (->> x (map #(get {"one" "1"
                                           "two" "2"
                                           "three" "3"
                                           "four" "4"
                                           "five" "5"
                                           "six" "6"
                                           "seven" "7"
                                           "eight" "8"
                                           "nine" "9"} % %)))]
                  x
                  #_(Long/parseLong (str (first x) (last x))))))
         #_(reduce + 0)))
(("two" "1" "nine") ("eight" "three") ("one" "2" "three") ("two" "3" "four") ("4" "nine" "eight" "seven" "2"))
(("7" "6" "two" "four") ("six" "three" "8" "six") ("3" "8" "nine" "five" "five") ("six" "three" "4" "eight") ("3"))

(-> (slurp "sample1.txt")
    (s/split #"\n")
    (->> (map (fn [s]
                (let [x (re-seq #"one|two|three|four|five|six|seven|eight|nine|[0-9]" s)
                      x (->> x (map #(get {"one" "1"
                                           "two" "2"
                                           "three" "3"
                                           "four" "4"
                                           "five" "5"
                                           "six" "6"
                                           "seven" "7"
                                           "eight" "8"
                                           "nine" "9"} % %)))]
                  (Long/parseLong (str (first x) (last x))))))
         #_(reduce + 0)))
(29 83 13 24 42 14 76)
(("two" "1" "nine") ("eight" "three") ("one" "2" "three") ("two" "3" "four") ("4" "nine" "eight" "seven" "2") ("one" "2" "3" "4") ("7" "six"))



(-> (slurp "sample1.txt")
    (s/split #"\n")
    (->> (map (fn [s]
                (let [x (re-seq #"one|two|three|four|five|six|seven|eight|nine|[0-9]" s)
                      x (->> x (map #(get {"one" "1"
                                           "two" "2"
                                           "three" "3"
                                           "four" "4"
                                           "five" "5"
                                           "six" "6"
                                           "seven" "7"
                                           "eight" "8"
                                           "nine" "9"} % %)))]
                  (Long/parseLong (str (first x) (last x))))))
         #_(reduce + 0)))
(29 83 13 24 42 14 76)

(-> (slurp "day1.txt")
    (s/split #"\n")
    (->> (map (fn [s]
                (let [x (re-seq #"one|two|three|four|five|six|seven|eight|nine|[0-9]" s)
                      x (->> x (map #(get {"one" "1"
                                           "two" "2"
                                           "three" "3"
                                           "four" "4"
                                           "five" "5"
                                           "six" "6"
                                           "seven" "7"
                                           "eight" "8"
                                           "nine" "9"} % %)))]
                  (Long/parseLong (str (first x) (last x))))))
         (reduce + 0)))
53355

(re-seq #"one|two|three|four|five|six|seven|eight|nine|[0-9]" "eighthree")
("eight")

(-> (slurp "day1.txt")
    (s/split #"\n")
    (->> (map (fn [s]
                (let [x (->> [#"one" #"two" #"three" #"four" #"five"
                              #"six" #"seven" #"eight" #"nine" #"[0-9]"]
                             (keep #(re-seq % s))
                             (apply concat)
                             (map #(get {"one" "1"
                                         "two" "2"
                                         "three" "3"
                                         "four" "4"
                                         "five" "5"
                                         "six" "6"
                                         "seven" "7"
                                         "eight" "8"
                                         "nine" "9"} % %)))]
                  (Long/parseLong (str (first x) (last x))))))
         (reduce + 0)))
43574

(-> (slurp "sample1.txt")
    (s/split #"\n")
    (->> (map (fn [s]
                (let [x (->> [#"one" #"two" #"three" #"four" #"five"
                              #"six" #"seven" #"eight" #"nine" #"[0-9]"]
                             (keep #(re-seq % s))
                             (apply concat)
                             (map #(get {"one" "1"
                                         "two" "2"
                                         "three" "3"
                                         "four" "4"
                                         "five" "5"
                                         "six" "6"
                                         "seven" "7"
                                         "eight" "8"
                                         "nine" "9"} % %)))]
                  (Long/parseLong (str (first x) (last x))))))
         #_(reduce + 0)))
(21 28 12 13 72 14 67)


(-> (slurp "sample1.txt")
    (s/split #"\n")
    (->> (map (fn [s]
                (let [x (->> [#"one" #"two" #"three" #"four" #"five"
                              #"six" #"seven" #"eight" #"nine" #"[0-9]"]
                             (keep #(re-seq % s))
                             (apply concat)
                             (map #(get {"one" "1"
                                         "two" "2"
                                         "three" "3"
                                         "four" "4"
                                         "five" "5"
                                         "six" "6"
                                         "seven" "7"
                                         "eight" "8"
                                         "nine" "9"} % %)))]
                  x
                  #_(Long/parseLong (str (first x) (last x))))))
         #_(reduce + 0)))
(("2" "9" "1") ("2" "3" "8") ("1" "3" "2") ("1" "2" "4" "3") ("7" "8" "9" "4" "2") ("1" "8" "2" "3" "4") ("6" "7"))
(21 28 12 13 72 14 67)


(-> (slurp "sample1.txt")
    (s/split #"\n")
    (->> (map (fn [s]
                (let [x (->> [#"one" #"two" #"three" #"four" #"five"
                              #"six" #"seven" #"eight" #"nine" #"[0-9]"]
                             (keep #(re-seq % s))
                             (apply concat)
                             (map #(get {"one" "1"
                                         "two" "2"
                                         "three" "3"
                                         "four" "4"
                                         "five" "5"
                                         "six" "6"
                                         "seven" "7"
                                         "eight" "8"
                                         "nine" "9"} % %)))]
                  x
                  #_(Long/parseLong (str (first x) (last x))))))
         #_(reduce + 0)))
(("2" "9" "1") ("2" "3" "8") ("1" "3" "2") ("1" "2" "4" "3") ("7" "8" "9" "4" "2") ("1" "8" "2" "3" "4") ("6" "7"))
(21 28 12 13 72 14 67)

(-> (slurp "sample1.txt")
    (s/split #"\n")
    (->> (map (fn [s]
                (->> (for [pat ["one" "two" "three" "four" "five"
                                "six" "seven" "eight" "nine"
                                "1" "2" "3" "4" "5" "6" "7" "8" "9"]
                           :let [x (s/index-of s pat)]
                           :when x]
                       [x pat])
                     sort
                     (map second)
                     (map #(get {"one" "1"
                                 "two" "2"
                                 "three" "3"
                                 "four" "4"
                                 "five" "5"
                                 "six" "6"
                                 "seven" "7"
                                 "eight" "8"
                                 "nine" "9"} % %))
                     ((juxt first last))
                     (apply str)
                     Long/parseLong)))
         (reduce + 0)))
281

(-> (slurp "day1.txt")
    (s/split #"\n")
    (->> (map (fn [s]
                (->> (for [pat ["one" "two" "three" "four" "five"
                                "six" "seven" "eight" "nine"
                                "1" "2" "3" "4" "5" "6" "7" "8" "9"]
                           :let [x (s/index-of s pat)]
                           :when x]
                       [x pat])
                     sort
                     (map second)
                     (map #(get {"one" "1"
                                 "two" "2"
                                 "three" "3"
                                 "four" "4"
                                 "five" "5"
                                 "six" "6"
                                 "seven" "7"
                                 "eight" "8"
                                 "nine" "9"} % %))
                     ((juxt first last))
                     (apply str)
                     Long/parseLong)))
         (reduce + 0)))
53380

(-> (slurp "day1.txt")
    (s/split #"\n")
    (->> (map (fn [s]
                (->> (for [pat ["one" "two" "three" "four" "five"
                                "six" "seven" "eight" "nine"
                                "1" "2" "3" "4" "5" "6" "7" "8" "9"]
                           :let [x (s/index-of s pat)]
                           :when x]
                       [x pat])
                     sort
                     (map second)
                     (map #(get {"one" "1"
                                 "two" "2"
                                 "three" "3"
                                 "four" "4"
                                 "five" "5"
                                 "six" "6"
                                 "seven" "7"
                                 "eight" "8"
                                 "nine" "9"} % %))
                     ((juxt first last))
                     (apply str)
                     Long/parseLong)))
         (reduce + 0)))
53380



(-> (slurp "day1.txt")
    (s/split #"\n")
    (->> (map (fn [s]
                (->> (for [pat ["one" "two" "three" "four" "five"
                                "six" "seven" "eight" "nine"
                                "1" "2" "3" "4" "5" "6" "7" "8" "9"]
                           :let [x (s/index-of s pat)]
                           :when x]
                       (loop [idxs [x]
                              x x]
                         (if-let [new-idx (s/index-of s pat (inc x))]
                           (recur (conj idxs new-idx) new-idx)
                           (map (fn [i] [i pat]) idxs))))
                     (apply concat)
                     sort
                     (map second)
                     (map #(get {"one" "1"
                                 "two" "2"
                                 "three" "3"
                                 "four" "4"
                                 "five" "5"
                                 "six" "6"
                                 "seven" "7"
                                 "eight" "8"
                                 "nine" "9"} % %))
                     ((juxt first last))
                     (apply str)
                     Long/parseLong)))
         (reduce + 0)))
53348
