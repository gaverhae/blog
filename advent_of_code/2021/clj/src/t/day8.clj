(ns t.day8
  (:require (clojure [string :as string]
                     [set :as set])))

(defn parse
  [lines]
  (->> lines
       (map (fn [s]
              (->> (string/split s #" ")
                   (split-with (comp not #{"|"}))
                   ((fn [[in out]]
                      [(->> in
                            (map (fn [s] (apply str (sort s))))
                            (sort-by count)
                            set)
                       (->> out
                            (map (fn [s] (apply str (sort s))))
                            (remove #{"|"}))])))))))

(defn permutations
  [s]
  (if (empty? s)
    (list ())
    (let [inits (reductions conj [] s)
          tails (take (inc (count s)) (iterate rest s))
          rotations (rest (map concat tails inits))]
      (mapcat (fn [[x & xs]] (map #(cons x %) (permutations xs)))
              rotations))))

(defn make-decoder
  []
  (let [ins #{"abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"}]
    (->> (permutations "abcdefg")
         (map (fn [p]
                (let [encode (into {} (map vector "abcdefg" p))
                      decode (into {} (map vector p "abcdefg"))]
                  [(->> ins
                        (map (fn [w] (->> w (map encode) sort (apply str))))
                        set)
                   (fn [words]
                     (->> words
                          (map (fn [w] (->> w (map decode) sort (apply str))))
                          (map {"abcefg" "0"
                                "cf" "1"
                                "acdeg" "2"
                                "acdfg" "3"
                                "bcdf" "4"
                                "abdfg" "5"
                                "abdefg" "6"
                                "acf" "7"
                                "abcdefg" "8"
                                "abcdfg" "9"})
                          (apply str)
                          (Long/parseLong)))])))
         (into {}))))

(defn part1
  [input]
  (->> input
       (map rest)
       (apply concat)
       (apply concat)
       (map count)
       (filter #{2 3 4 7})
       count))

(defn part2
  [input]
  (let [decoder (make-decoder)]
    (->> input
         (map (fn [[ins outs]]
                ((decoder ins) outs)))
         (reduce + 0))))
