(ns t.day10)

(defn parse
  [lines]
  (->> lines))

(def matching
  {\( \), \{ \}, \[ \], \< \>})

(defn part1
  [input]
  (->> input
       (map (fn [s]
              (loop [stack [(first s)]
                     [c & r] (rest s)]
                (cond (and (nil? c) (empty? stack)) :correct
                      (nil? c) :incomplete
                      (contains? matching c) (recur (cons c stack) r)
                      (= (matching (first stack)) c) (recur (rest stack) r)
                      :else ({\) 3, \] 57, \} 1197, \> 25137} c)))))
       (filter int?)
       (reduce +)))

(defn part2
  [input]
  (->> input
       (map (fn [s]
              (loop [stack [(first s)]
                     [c & r] (rest s)]
                (cond (and (nil? c) (empty? stack)) :correct
                      (nil? c) (->> stack
                                    (map {\( 1, \[ 2, \{ 3, \< 4})
                                    (reduce (fn [acc el]
                                              (+ (* 5 acc)  el))))
                      (contains? matching c) (recur (cons c stack) r)
                      (= (matching (first stack)) c) (recur (rest stack) r)
                      :else :corrupted))))
       (filter int?)
       sort
       ((fn [scores]
          (nth scores (/ (dec (count scores)) 2))))))

