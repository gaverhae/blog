(ns t.core
  (:gen-class)
  (:require [clojure.string :as string]
            [criterium.core :as crit]
            [t.day1 :as day1]
            [t.day2 :as day2]
            [t.day3 :as day3]
            [t.day4 :as day4]))

(defn -main
  [& args]
  (doseq [i [1 2 3 4]]
    (let [parse (ns-resolve 't.core (symbol (str "day" i) "parse"))
          part1 (ns-resolve 't.core (symbol (str "day" i) "part1"))
          part2 (ns-resolve 't.core (symbol (str "day" i) "part2"))
          input (parse (string/split-lines (slurp (str "data/day" i))))]
      (print (format (str "Day %02d, part 1: %15d\n"
                          "        part 2: %15d\n")
                     i
                     (part1 input)
                     (part2 input)))
      (when (= i 4)
        (flush)
        #_(read-line)
        (crit/bench (part2 input)))))
  (flush))
