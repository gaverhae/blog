(ns t.day19
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [instaparse.core :as insta]))

(defn parse-grammar
  [g]
  (let [parse-nums (fn [s] (->> (string/split s #" ")
                                (map (fn [n] (Long/parseLong n)))
                                (cons :seq)
                                vec))]
    (->> g
         (map (fn [r]
                (let [colon (string/index-of r \:)]
                  [(Long/parseLong (subs r 0 colon))
                   (subs r (+ 2 colon))])))
         (map (fn [[idx ^String r]]
                [idx
                 (if-let [[_ alt1 alt2] (re-matches #"(.*) \| (.*)" r)]
                   [:or (parse-nums alt1) (parse-nums alt2)]
                   (if (.startsWith r "\"")
                     [:terminal (.charAt r 1)]
                     (parse-nums r)))]))
         (into {}))))

(defn parse
  [lines]
  (let [[grammar _ lines] (partition-by #{""} lines)]
    {:grammar (->> grammar parse-grammar)
     :raw-grammar grammar
     :lines lines}))

(defn apply-grammar
  [g rule s]
  (match rule
    [:or alt1 alt2] (or (apply-grammar g alt1 s)
                        (apply-grammar g alt2 s))
    [:seq & rs] (reduce (fn [s r]
                          (when s (apply-grammar g (g r) s)))
                        s
                        rs)
    [:terminal c] (when (= c (first s)) (subs s 1))))

(defn check
  [g s]
  (= "" (apply-grammar g (g 0) s)))

(defn part1
  [input]
  (let [valid? (fn [line] (check (:grammar input) line))]
    (->> (:lines input)
         (filter valid?)
         count)))

(defn part2
  [input]
  (let [g (->> (:raw-grammar input)
               (map (fn [^String r]
                      (cond (.startsWith r "8: ") "8: 42 | 42 8"
                            (.startsWith r "11: ") "11: 42 31 | 42 11 31"
                            :else r)))
               (cons "S: 0"))
        p (insta/parser (string/join "\n" g))
        valid? (fn [line] (not-empty (insta/parses p line)))]
    (->> (:lines input)
         (filter valid?)
         count)))
