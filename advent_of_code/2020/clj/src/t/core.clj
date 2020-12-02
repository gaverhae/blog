(ns t.core
  (:require clojure.string))

(defn sum-2020
  [ints]
  (let [len (count ints)]
    (->> (for [x (range len)
               y (range x len)]
           [(ints x) (ints y)])
         (filter #(= 2020 (+ (% 0) (% 1))))
         first)))

(defn day-1-part-1
  []
  (let [data (->> (slurp "data/day-1-part-1")
                  (clojure.string/split-lines)
                  (map #(Long/parseLong %))
                  vec)
        [x y] (sum-2020 data)]
    (* x y)))

(defn day-1-part-2
  []
  (let [data (->> (slurp "data/day-1-part-1")
                  (clojure.string/split-lines)
                  (map #(Long/parseLong %))
                  vec)]
    (first (for [x (range (count data))
                 y (range x (count data))
                 z (range y (count data))
                 :let [dx (data x)
                       dy (data y)
                       dz (data z)]
                 :when (= 2020 (+ dx dy dz))]
                         (* dx dy dz)))))

(defn parse-day-2
  [f]
  (->> f
       (clojure.string/split-lines)
       (map #(clojure.string/split % #" |: |-"))
       (map (fn [[min max c pw]]
              [(Long/parseLong min)
               (Long/parseLong max)
               (first c)
               pw]))))

(defn count-good-passwords
  [lines]
  (->> lines
       (filter (fn [[min max c pw]]
                 (<= min
                     (count (filter #(= c %) pw))
                     max)))
       count))

(defn count-good-passwords-2
  [lines]
  (->> lines
       (filter (fn [[min max c ^String pw]]
                 (let [a (= c (.charAt pw (dec min)))
                       b (= c (.charAt pw (dec max)))]
                   (if a (not b) b))))
       count))

(defn day-2-part-1
  []
  (->> (slurp "data/day2")
       parse-day-2
       count-good-passwords))

(comment

  (day-1-part-1)
712075

(day-1-part-2)
145245270

(day-2-part-1)
666

(->> (slurp "data/day2")
     parse-day-2
     count-good-passwords-2)
348

  )

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
