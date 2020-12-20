(ns t.day19
  (:require [clojure.string :as string]
            [instaparse.core :as insta]))

(defn parse
  [lines]
  (let [[grammar _ lines] (partition-by #{""} lines)]
    {:grammar (cons "S: 0" grammar)
     :lines lines}))


(defn part1
  [input]
  (let [p (insta/parser (string/join "\n" (:grammar input)))
        valid? (fn [line] (not-empty (insta/parses p line)))]
    (->> (:lines input)
         (filter valid?)
         count)))

(defn part2
  [input]
  (let [g (->> (:grammar input)
               (map (fn [^String r]
                      (cond (.startsWith r "8: ") "8: 42 | 42 8"
                            (.startsWith r "11: ") "11: 42 31 | 42 11 31"
                            :else r))))
        p (insta/parser (string/join "\n" g))
        valid? (fn [line] (not-empty (insta/parses p line)))]
    (->> (:lines input)
         (filter valid?)
         count)))
