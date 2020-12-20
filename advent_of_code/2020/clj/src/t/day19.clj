(ns t.day19
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]))

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
     :lines lines}))

(defn apply-grammar
  [g rule s]
  (match rule
    [:or alt1 alt2] (concat (apply-grammar g alt1 s)
                            (apply-grammar g alt2 s))
    [:seq & rs] (reduce (fn [ss r]
                          (mapcat (fn [s] (when s (apply-grammar g (g r) s)))
                                  ss))
                        [s]
                        rs)
    [:terminal c] (when (= c (first s)) [(subs s 1)])))

(defn check
  [g s]
  (some #{""} (apply-grammar g (g 0) s)))

(defn part1
  [input]
  (let [valid? (fn [line] (check (:grammar input) line))]
    (->> (:lines input)
         (filter valid?)
         count)))

(defn part2
  [input]
  (part1 (update input :grammar assoc 8 [:or [:seq 42] [:seq 42 8]]
                                      11 [:or [:seq 42 31] [:seq 42 11 31]])))
