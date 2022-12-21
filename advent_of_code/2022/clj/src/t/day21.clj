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

(defn neval
  [env op]
  (match op
    [:lit i] i
    [:op f n1 n2] (({"+" +, "-" -, "*" *, "/" /} f)
                   (neval env (env n1))
                   (neval env (env n2)))))

(defn part1
  [input]
  (neval input (input "root")))

(defn part2
  [input]
  (let [left (get-in input ["root" 2])
        right (get-in input ["root" 3])
        contains-humn? (fn r [env n]
                         (match n
                           [:lit _] false
                           [:op _ a b] (or (= a "humn")
                                           (= b "humn")
                                           (r env (env a))
                                           (r env (env b)))))
        [tbd target] (if (contains-humn? input (input left)) [left right] [right left])
        target-value (neval input (input target))
        solve (fn r [env target n]
                (if (= "humn" n)
                  target
                  (let [[_ f n1 n2] (env n)
                        [h nh ordered] (if (or (contains-humn? env (env n1))
                                               (= "humn" n1))
                                         [n1 n2 true]
                                         [n2 n1 false])
                        v (neval env (env nh))]
                    (prn [n := target := (env n) :where nh := v])
                    (match [f ordered]
                      ["*" _] (r env (/ target v) h)
                      ["+" _] (r env (- target v) h)
                      ["/" true] (r env (* target v) h)
                      ["-" true] (r env (+ target v) h)
                      ["/" false] (r env (/ v target) h)
                      ["-" false] (r env (- v target) h)))))]
    (solve input target-value tbd)))

(lib/check
  [part1 sample] 152
  [part1 puzzle] 72664227897438
  [part2 sample] 301
  [part2 puzzle] 3916491093817)
