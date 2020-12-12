(ns t.day12)

(defn parse
  [lines]
  (map (fn [line]
         (let [n (Long/parseLong (subs line 1))]
           (case (first line)
             \N [:move [0 n]]
             \S [:move [0 (- n)]]
             \E [:move [n 0]]
             \W [:move [(- n) 0]]
             \L [:turn (quot n 90)]
             \R [:turn (mod (- (quot n 90)) 4)]
             \F [:forward n])))
       lines))

(defn part1
  [input]
  (->> input
       (reduce (fn [[pos dir] [m arg]]
                 (case m
                   :move [(map + pos arg) dir]
                   :turn [pos (mod (+ arg dir) 4)]
                   :forward (let [[dx dy] ([[1 0] [0 1] [-1 0] [0 -1]] dir)
                                  [x y] pos]
                              [[(+ x (* dx arg)) (+ y (* dy arg))] dir])))

               [[0 0] 0])
       first
       (map #(Math/abs (long %)))
       (apply +)))

(defn part2
  [input]
  (->> input
       (reduce (fn [[[x y] [dx dy]] [cmd arg]]
                 #_(prn [[x y] [dx dy]])
                 (case cmd
                   :move [[x y] (mapv + [dx dy] arg)]
                   :turn [[x y] ([[dx dy] [(- dy) dx] [(- dx) (- dy)] [dy (- dx)]] arg)]
                   :forward [[(+ x (* arg dx)) (+ y (* arg dy))] [dx dy]]))
               [[0 0] [10 1]])
       first
       (map #(Math/abs (long %)))
       (apply +)))
