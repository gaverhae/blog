(ns t.core
  (:gen-class)
  (:require [clojure.string :as string]
            [criterium.core :as crit]))

(defn bench
  [f]
  (->> (crit/benchmark (f) {}) :mean first))

(defmacro time-ms
  [exp]
  `(let [start# (System/nanoTime)
         r# ~exp
         end# (System/nanoTime)]
     (quot (- end# start#) 1000000)))

(defn fmt
  [f & args]
  (println (apply format f args)))

(defn -main
  [& args]
  (let [days (->> (range 25)
                  (map inc)
                  (mapv (fn [day]
                          (let [ns (symbol (str "t.day" day))
                                _ (require ns)
                                parse (ns-resolve ns (symbol "parse"))
                                part1 (ns-resolve ns (symbol "part1"))
                                part2 (ns-resolve ns (symbol "part2"))
                                sam (string/split-lines (slurp (str "data/sample" day)))
                                psam (parse sam)
                                data (string/split-lines (slurp (str "data/day" day)))
                                input (parse data)]
                            [day
                             {:total #(time-ms (do (parse data) (part1 input) (part2 input) nil))
                              :parse-sam #(parse sam)
                              :parse #(parse data)
                              :part1 #(part1 input)
                              :part1-sam #(part1 psam)
                              :part2 #(part2 input)
                              :part2-sam #(part2 psam)}])))
                  (into {}))]
    (do
      (->> days
           (map (fn [[day {:keys [total]}]]
                  [(total) day]))
           sort
           reverse
           (map (fn [[t d]]
                  (println (format "Day %02d: %15dms" d t))))
           doall))
    #_(doseq [[day {:keys [parse part1 part2 parse-sam part1-sam part2-sam]}]
            (select-keys days [23])]
      (fmt "Day %02d, parse (sample): %8.3fs" day (bench parse-sam))
      (fmt "Day %02d, parse (input):  %8.3fs" day (bench parse))
      (fmt "Day %02d, part1 (sample): %8.3fs" day (bench part1-sam))
      (fmt "Day %02d, part1 (input):  %8.3fs" day (bench part1))
      (fmt "Day %02d, part2 (sample): %8.3fs" day (bench part2-sam))
      (fmt "Day %02d, part2 (input):  %8.3fs" day (bench part2)))
    #_(let [to-profile (get-in days [23 :part2])]
      (println (format "Single run: %d." (time-ms (to-profile))))
      (println "Waiting for profiler.")
      (read-line)
      (println (format "%.3f" (bench #(to-profile)))))))
