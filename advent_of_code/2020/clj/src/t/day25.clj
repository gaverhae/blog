(ns t.day25)

(defn parse
  [lines]
  (->> lines
       (map #(Long/parseLong %))
       vec))

(defn transform
  [subj]
  (iterate (fn [n] (rem (* subj n) 20201227)) 1))

(defn part1
  [[card door]]
  (let [card-n (->> (transform 7)
                    (map-indexed vector)
                    (filter (fn [[idx n]] (= n card)))
                    ffirst)]
    (nth (transform door) card-n)))

(defn part2
  [input])
