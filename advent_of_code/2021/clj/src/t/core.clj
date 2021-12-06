(ns t.core
  (:gen-class)
  (:require [clojure.string :as string]
            [criterium.core :as crit]
            [t.day1 :as day1]
            [t.day2 :as day2]
            [t.day3 :as day3]
            [t.day4 :as day4]
            [t.day5 :as day5]
            [t.day6 :as day6]))

(defn bench
  [f]
  (->> (crit/benchmark (f) {}) :mean first))

(defn -main
  [& args]
  (doseq [i [1 2 3 4 5 6]]
    (let [parse (ns-resolve 't.core (symbol (str "day" i) "parse"))
          part1 (ns-resolve 't.core (symbol (str "day" i) "part1"))
          part2 (ns-resolve 't.core (symbol (str "day" i) "part2"))
          data (string/split-lines (slurp (str "data/day" i)))
          input (parse data)]
      (print (format (str "Day %02d, part 1: %15d\n"
                          "        part 2: %15d\n")
                     i
                     (part1 input)
                     (part2 input)))
      (when false
        (print (format (str "        parse -> %14.3f\n"
                            "        part1 -> %14.3f\n"
                            "        part2 -> %14.3f\n")
                       (bench #(parse data))
                       (bench #(part1 input))
                       (bench #(part2 input)))))
      (when false
        (println "Waiting for profiler.")
        (read-line)
        (crit/bench (parse data))
        (crit/bench (part1 input))
        (crit/bench (part2 input)))
      (flush))))
