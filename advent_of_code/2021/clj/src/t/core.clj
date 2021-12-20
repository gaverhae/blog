(ns t.core
  (:gen-class)
  (:require [clojure.string :as string]
            [criterium.core :as crit]))

(defn bench
  [f]
  (->> (crit/benchmark (f) {}) :mean first))

(defn fmt
  [f i]
  (print (format f i))
  (flush))

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
      (when (= i 9) ;; 15 17 18 19 20
        (fmt "Day %02d," i)
        (fmt " part 1: %15d\n" (part1 input))
        (fmt "        part 2: %15d\n" (part2 input))
        (fmt "        parse -> %14.3f\n" (bench #(parse data)))
        (fmt "        part1 -> %14.3f\n" (bench #(part1 input)))
        (fmt "        part2 -> %14.3f\n" (bench #(part2 input)))
        (fmt "        all   -> %14.3f\n" (bench #(do (parse data)
                                                     (part1 input)
                                                     (part2 input)))))
      (when false
        (println "Waiting for profiler.")
        (read-line)
        (bench (parse data))
        (bench (part1 input))
        (bench (part2 input)))
      (flush))))
