(ns t.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

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

(defn parse5
  [s]
  (->> (string/split-lines s)
       (map #(map {\F \0, \B \1, \L \0, \R \1} %))
       (map #(-> [(apply str (take 7 %))
                  (apply str (drop 7 %))]))
       (map #(map (fn [s] (Long/parseLong s 2)) %))))

(comment
  (->> (slurp "data/day5")
       parse5
       (map (fn [[r c]] (+ (* r 8) c)))
       (apply max))
826

  (->> (slurp "data/day5")
       parse5
       (map (fn [[r c]] (+ (* r 8) c)))
       sort
       (partition 2 1)
       (filter (fn [[p n]] (= 2 (- n p)))))
((677 679))

  )



(comment

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

  )
