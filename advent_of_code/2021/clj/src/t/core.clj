(ns t.core
  (:gen-class)
  (:require [clojure.string :as string]
            [criterium.core :as crit]))

(defn bench
  [f]
  (->> (crit/benchmark (f) {}) :mean first))

(defmacro fmt
  [f i]
  `(let [start# (System/nanoTime)
         r# ~i
         end# (System/nanoTime)]
     (print (format ~f (if (integer? r#) r# 0) (quot (- end# start#) 1000000)))
     (flush)))

(defn -main
  [& args]
  (doseq [day (map inc (range 20))]
    (let [ns (symbol (str "t.day" day))
          _ (require ns)
          parse (ns-resolve ns (symbol "parse"))
          part1 (ns-resolve ns (symbol "part1"))
          part2 (ns-resolve ns (symbol "part2"))
          data (string/split-lines (slurp (str "data/day" day)))
          input (parse data)]
      (when true
        (println (format "Day %02d:" day))
        (fmt "          part 1: %15d (%dms)\n" (part1 input))
        (fmt "          part 2: %15d (%dms)\n" (part2 input))
        #_(fmt "          parse -> %14.3f (%dms)\n" (bench #(parse data)))
        #_(fmt "          part1 -> %14.3f (%dms)\n" (bench #(part1 input)))
        #_(fmt "          part2 -> %14.3f (%dms)\n" (bench #(part2 input)))
        #_(fmt "          all   -> %14.3f (%dms)\n" (bench #(do (parse data)
                                                                  (part1 input)
                                                                  (part2 input)))))
      (when false
        (println "Waiting for profiler.")
        (read-line)
        (bench (parse data))
        (bench (part1 input))
        (bench (part2 input)))
      (flush))))
