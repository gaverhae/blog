(ns t.core
  (:gen-class)
  (:require [clojure.string :as string]
            [criterium.core :as crit]))

(defn bench
  [f]
  (->> (crit/benchmark (f) {}) :mean first))

(defn -main
  [& args]
  (doseq [i (map inc (range 20))]
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
      (when true
        (print (format (str "        parse -> %14.3f\n")
                       (bench #(parse data))))
        (flush)
        (print (format (str "        part1 -> %14.3f\n")
                       (bench #(part1 input))))
        (flush)
        (print (format (str "        part2 -> %14.3f\n")
                       (bench #(part2 input))))
        (flush)
        (print (format (str "        all   -> %14.3f\n")
                       (bench #(do (parse data)
                                   (part1 input)
                                   (part2 input)))))
        (flush))
      #_(when (= i 12)
        (println "Waiting for profiler.")
        (read-line)
        #_(crit/bench (parse data))
        #_(crit/bench (part1 input))
        (crit/bench (part2 input)))
      (flush))))
