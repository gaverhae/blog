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
  [f i]
  (println (format f i)))

(defn -main
  [& args]
  (let [days (->> (range 24)
                  (map inc)
                  (mapv (fn [day]
                          (let [ns (symbol (str "t.day" day))
                                _ (require ns)
                                parse (ns-resolve ns (symbol "parse"))
                                part1 (ns-resolve ns (symbol "part1"))
                                part2 (ns-resolve ns (symbol "part2"))
                                data (string/split-lines (slurp (str "data/day" day)))
                                input (parse data)]
                            [day
                             {:total #(time-ms (do (parse data) (part1 input) (part2 input) nil))
                              :parse #(parse data)
                              :part1 #(part1 input)
                              :part2 #(part2 input)}])))
                  (into {}))]
    #_(do
      (->> days
           (map (fn [[day {:keys [total]}]]
                  [(total) day]))
           sort
           reverse
           (map (fn [[t d]]
                  (println (format "Day %02d: %15dms" d t))))
           doall))
    #_(doseq [[day {:keys [parse part1 part2]}] days]
      (fmt "Day %02d, parse: %.3fms" day (bench (parse)))
      (fmt "Day %02d, part1: %.3fms" day (bench (part1)))
      (fmt "Day %02d, part2: %.3fms" day (bench (part2))))
    (let [to-profile (get-in days [24 :part1])]
      (println (format "Single run: %d." (to-profile)))
      #_(println "Waiting for profiler.")
      #_(read-line)
      #_(println (format "%.3f" (bench #(to-profile)))))))
