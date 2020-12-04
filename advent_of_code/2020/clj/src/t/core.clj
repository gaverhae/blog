(ns t.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn sum-2020
  [ints]
  (let [len (count ints)]
    (->> (for [x (range len)
               y (range x len)]
           [(ints x) (ints y)])
         (filter #(= 2020 (+ (% 0) (% 1))))
         first)))

(defn day-1-part-1
  []
  (let [data (->> (slurp "data/day-1-part-1")
                  (string/split-lines)
                  (map #(Long/parseLong %))
                  vec)
        [x y] (sum-2020 data)]
    (* x y)))

(defn day-1-part-2
  []
  (let [data (->> (slurp "data/day-1-part-1")
                  (string/split-lines)
                  (map #(Long/parseLong %))
                  vec)]
    (first (for [x (range (count data))
                 y (range x (count data))
                 z (range y (count data))
                 :let [dx (data x)
                       dy (data y)
                       dz (data z)]
                 :when (= 2020 (+ dx dy dz))]
                         (* dx dy dz)))))

(defn parse-day-2
  [f]
  (->> f
       (string/split-lines)
       (map #(string/split % #" |: |-"))
       (map (fn [[min max c pw]]
              [(Long/parseLong min)
               (Long/parseLong max)
               (first c)
               pw]))))

(defn count-good-passwords
  [lines]
  (->> lines
       (filter (fn [[min max c pw]]
                 (<= min
                     (count (filter #(= c %) pw))
                     max)))
       count))

(defn count-good-passwords-2
  [lines]
  (->> lines
       (filter (fn [[min max c ^String pw]]
                 (let [a (= c (.charAt pw (dec min)))
                       b (= c (.charAt pw (dec max)))]
                   (if a (not b) b))))
       count))

(defn day-2-part-1
  []
  (->> (slurp "data/day2")
       parse-day-2
       count-good-passwords))

(defn day-3-parse
  [s]
  (->> s
       (string/split-lines)
       (map #(map {\. 0 \# 1} %))
       (map cycle)))

(defn slope
  [[right down] geo]
  (loop [n 0
         pos geo]
    (if (empty? pos)
      n
      (recur (+ n (ffirst pos))
             (drop down (map #(drop right %) pos))))))

(defn day-3-part-2
  [data]
  (let [terrain (day-3-parse data)]
    (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
         (map #(slope % terrain))
         (reduce *))))

(defn parse4
  [s]
  (->> (string/split-lines s)
       (map (fn [line] (->> (string/split line #" ")
                            (map (fn [word] (string/split word #":"))))))
       (partition-by #(= % [[""]]))
       (remove #(= % [[[""]]]))
       (map #(reduce concat %))
       (map #(reduce (fn [acc [k v]] (assoc acc (keyword k) v)) {} %))))

(defn d41
  [maps]
  (->> maps
       (filter #(set/subset? #{:byr :iyr :eyr :hgt :hcl :ecl :pid} (set (keys %))))
       count))

(defn d42
  [maps]
  (->> maps
       (filter #(set/subset? #{:byr :iyr :eyr :hgt :hcl :ecl :pid} (set (keys %))))
       (filter (fn [p]
                 (and (re-matches #"\d\d\d\d" (:byr p))
                      (<= 1920 (Long/parseLong (:byr p)) 2002)
                      (re-matches #"\d\d\d\d" (:iyr p))
                      (<= 2010 (Long/parseLong (:iyr p)) 2020)
                      (re-matches #"\d\d\d\d" (:eyr p))
                      (<= 2020 (Long/parseLong (:eyr p)) 2030)
                      (let [[_ n units] (re-matches #"(\d+)(in|cm)" (:hgt p))]
                        (case units
                          "cm" (<= 150 (Long/parseLong n) 193)
                          "in" (<= 59 (Long/parseLong n) 76)
                          false))
                      (re-matches #"#[0-9a-f]{6}" (:hcl p))
                      (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} (:ecl p))
                      (re-matches #"\d{9}" (:pid p)))))
       count))


(comment

  (day-1-part-1)
712075

(day-1-part-2)
145245270

(day-2-part-1)
666

(->> (slurp "data/day2")
     parse-day-2
     count-good-passwords-2)
670

(->> (slurp "data/day3")
     (day-3-parse)
     (slope [3 1]))
214

(->> (slurp "data/day3")
     (day-3-part-2))
8336352024

(->> (slurp "data/day4")
     parse4
     d41)
210

(->> (slurp "data/day4")
     parse4
     d42)
132

  )

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
