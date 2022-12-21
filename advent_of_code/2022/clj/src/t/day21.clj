(ns t.day21
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
     (map (fn [line]
            (if-let [[_ n i] (re-matches #"([a-z]+): (\d+)" line)]
              [n [:lit (->long i)]]
              (let [[_ n op1 f op2] (re-matches #"([a-z]+): ([a-z]+) (\+|-|\*|/) ([a-z]+)" line)]
                [n [:op f op1 op2]]))))
     (into {})))

(defn part1
  [input]
  (let [ev (fn ev [env op]
             (match op
               [:lit i] i
               [:op f n1 n2] (({"+" +, "-" -, "*" *, "/" /} f)
                              (ev env (env n1))
                              (ev env (env n2)))))]
    (ev input (input "root"))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 152
  [part1 puzzle] 72664227897438
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
