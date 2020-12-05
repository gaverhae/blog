(ns t.day4
  (:require [clojure.set :as set]
            [clojure.string :as string]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line] (->> (string/split line #" ")
                            (map (fn [word] (string/split word #":"))))))
       (partition-by #(= % [[""]]))
       (remove #(= % [[[""]]]))
       (map #(reduce concat %))
       (map #(reduce (fn [acc [k v]] (assoc acc (keyword k) v)) {} %))))

(defn has-required-keys?
  [p]
  (set/subset? #{:byr :iyr :eyr :hgt :hcl :ecl :pid} (set (keys p))))

(defn part1
  [input]
  (->> input
       (filter has-required-keys?)
       count))

(defn part2
  [input]
  (->> input
       (filter (fn [p]
                 (and (has-required-keys? p)
                      (re-matches #"\d\d\d\d" (:byr p))
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
