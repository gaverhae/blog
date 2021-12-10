(ns t.day10)

(let [matching {\( \), \{ \}, \[ \], \< \>}
      incorrect-score {\) 3, \] 57, \} 1197, \> 25137}
      incomplete-score (fn [stack]
                         (->> stack
                              (map {\( 1, \[ 2, \{ 3, \< 4})
                              (reduce (fn [acc el]
                                        (+ (* 5 acc)  el)))))]
  (defn parse-line
    [line]
    (loop [stack [(first line)]
           [c & r] (rest line)]
      (cond (and (nil? c) (empty? stack)) [:correct]
            (nil? c) [:incomplete (incomplete-score stack)]
            (contains? matching c) (recur (cons c stack) r)
            (= (matching (first stack)) c) (recur (rest stack) r)
            :else [:incorrect (incorrect-score c)]))))

(defn median
  [ls]
  (nth (sort ls) (quot (count ls) 2)))

(defn parse
  [lines]
  (->> lines
       (map parse-line)))

(defn part1
  [input]
  (->> input
       (filter #(= :incorrect (first %)))
       (map second)
       (reduce +)))

(defn part2
  [input]
  (->> input
       (filter #(= :incomplete (first %)))
       (map second)
       median))
