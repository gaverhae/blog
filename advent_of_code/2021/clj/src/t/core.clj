(ns t.core
  (:gen-class)
  (:require [clojure.string :as string]
            [criterium.core :as crit]))

(defn bench
  [f]
  (->> (crit/benchmark (f) {}) :mean first))

(defn -main
  [& args]
  (doseq [i (map inc (range 7))]
    (let [ns (symbol (str "t.day" i))
          _ (require ns)
          parse (ns-resolve ns (symbol "parse"))
          part1 (ns-resolve ns (symbol "part1"))
          part2 (ns-resolve ns (symbol "part2"))
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
