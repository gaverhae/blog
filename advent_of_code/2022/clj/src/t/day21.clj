(ns t.day21
  (:require [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(def parser
  (insta/parser
    "<S> := lit | comp
    lit := name <': '> num
    comp := name <': '> name <' '> op <' '> name
    <num> := #'\\d+'
    <name> := #'[a-z]+'
    <op> := '+' | '-' | '/' | '*'"))

(defn parse
  [lines]
  (->> lines
       (map (comp first parser))
       (map (fn [l]
              (match l
                [:lit n v] [n [:lit (->long v)]]
                [:comp r n1 op n2] [r [:op op n1 n2]])))
       (into {})))

(defn neval
  [env]
  (fn r [op]
    (match op
      [:lit i] i
      [:op f n1 n2] (({"+" +, "-" -, "*" *, "/" /} f)
                     (r (env n1))
                     (r (env n2))))))

(defn part1
  [env]
  ((neval env) (env "root")))

(defn part2
  [env]
  (let [nev (neval env)
        left (get-in env ["root" 2])
        right (get-in env ["root" 3])
        contains-humn? (fn r [n]
                         (match n
                           [:lit _] false
                           [:op _ a b] (or (= a "humn")
                                           (= b "humn")
                                           (r (env a))
                                           (r (env b)))))
        [tbd target] (if (contains-humn? (env left)) [left right] [right left])
        target-value (nev (env target))
        solve (fn r [target n]
                (if (= "humn" n)
                  target
                  (let [[_ f n1 n2] (env n)
                        [h nh ordered] (if (or (contains-humn? (env n1))
                                               (= "humn" n1))
                                         [n1 n2 true]
                                         [n2 n1 false])
                        v (nev (env nh))]
                    (match [f ordered]
                      ["*" _] (r (/ target v) h)
                      ["+" _] (r (- target v) h)
                      ["/" true] (r (* target v) h)
                      ["-" true] (r (+ target v) h)
                      ["/" false] (r (/ v target) h)
                      ["-" false] (r (- v target) h)))))]
    (solve target-value tbd)))

(lib/check
  [part1 sample] 152
  [part1 puzzle] 72664227897438
  [part2 sample] 301
  [part2 puzzle] 3916491093817)
