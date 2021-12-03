(ns t.day3)

(def flip {\0 \1, \1 \0})

(defn transpose
  [s]
  (apply mapv vector s))

(defn most-frequent
  [s]
  (->> s frequencies (sort-by (juxt val key)) last key))

(defn bins->int
  [s]
  (Long/parseLong (apply str s) 2))

(defn binprod
  [& s]
  (->> s (map bins->int) (reduce * 1)))

(defn parse
  [lines]
  lines)

(defn part1
  [input]
  (let [gamma (->> input
                   transpose
                   (map most-frequent))]
    (binprod gamma
             (map flip gamma))))

(most-frequent (map first [[\0 \0 \1]]))

(defn part2
  [input]
  (let [most-common-bit #(most-frequent (map first %))
        rating (fn [f]
                 (loop [lines input
                        s ""]
                   (if (= 1 (count lines))
                     (str s (first lines))
                     (let [next-bit (f lines)]
                       (recur (->> lines
                                   (filter (fn [r] (= next-bit (first r))))
                                   (map #(subs % 1)))
                              (str s next-bit))))))]
    (binprod (rating most-common-bit)
             (rating (comp flip most-common-bit)))))
