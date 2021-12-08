(ns t.day8
  (:require (clojure [string :as string]
                     [set :as set])))

(defn parse
  [lines]
  (->> lines
       (map (fn [s]
              (-> s
                  (string/split #" ")
                  (->> (split-with (comp not #{"|"}))
                       (map (fn [x] (remove #{"|"} x)))))))))

(defn decode
  [[inputs outputs]]
  (let [e (->> "abcdefg"
               (filter (fn [c]
                         (->> inputs
                              (filter #(contains? (set %) c))
                              count
                              (== 4))))
               set)
        b (->> "abcdefg"
               (filter (fn [c]
                         (->> inputs
                              (filter #(contains? (set %) c))
                              count
                              (== 6))))
               set)
        d (set/intersection
            (->> "abcdefg"
                 (filter (fn [c]
                           (->> inputs
                                (filter #(contains? (set %) c))
                                count
                                (== 7))))
                 set)
            (->> inputs
                 (filter #(= (count %) 4))
                 first
                 set))
        g (set/difference
            (->> "abcdefg"
                 (filter (fn [c]
                           (->> inputs
                                (filter #(contains? (set %) c))
                                count
                                (== 7))))
                 set)
            d)
        a (set/difference
            (set "abcdefg")
            e b d g
            (->> inputs
                 (filter #(= (count %) 2))
                 first
                 set))
        f (set/difference
            (->> inputs
                 (filter #(= (count %) 6))
                 (filter #(set/subset? d (set %)))
                 (filter #(set/subset? e (set %)))
                 first
                 set)
            a b d e g)

        c (set/difference
            (set "abcdefg")
            a b d e f g)
        decoder (->> [a b c d e f g]
                     (map first)
                     (apply str)
                     (map vector "abcdefg")
                     (map (comp vec reverse))
                     (into {}))]
    (->> outputs
         (map (fn [w] (->> w (map decoder) sort (apply str))))
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
         (Long/parseLong))))


;  (loop [unknowns inputs
;         decoder (into {} (map (fn [x] [x (set "abcdefg")]) "abcdefg"))]
;    (prn [unknowns decoder])
;
;    (if (every? #(= 1 (count %)) (vals decoder))
;      [decoder (->> outputs
;           (map #(map decoder %))
;           (map sort)
;           (map #(apply str %))
;           #_(map {[0 1 2 4 5 6] 0
;                 [2 5] 1
;                 [0 2 3 4 6] 2
;                 [0 2 3 5 6] 3
;                 [1 2 3 5] 4
;                 [0 1 3 5 6] 5
;                 [0 1 3 4 5 6] 6
;                 [0 2 5] 7
;                 [0 1 2 3 4 5 6] 8
;                 [0 1 2 3 5 6] 9}))]
;      (let [f (first unknowns)]
;        (case (count f)
;          ;; 1
;          2 (recur (rest unknowns)
;                   (reduce (fn [m k]
;                             (update m k set/intersection (set "cf")))
;                             decoder
;                             f))
;          ;; 7
;          3 (recur (rest unknowns)
;                   (reduce (fn [m k]
;                             (update m k set/intersection (set "acf"))
;                             decoder
;                             f)))
;          ;; 4
;          4 (recur (rest unknowns)
;                   (reduce (fn [m k]
;                             (update m k set/intersection (set "bcdf")))
;                           decoder
;                           f))
;          ;; 8
;          7 (recur (rest unknowns)
;                   decoder)
;          ;;
;
;
;
;          decoder
;          )))))
;          4 (recur (rest unknowns)
;                   (into decoder (map vector f "bcdf")))
;          7 (recur (rest unknowns)
;                   (into decoder (map vector f "abcdefg")))
;          ::decoder)))))

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
  (->> input
       (map decode)
       (reduce + 0)))
