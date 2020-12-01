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

(comment

  (day-1-part-1)
712075

  )

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
