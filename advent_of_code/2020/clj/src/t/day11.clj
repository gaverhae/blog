(ns t.day11)

(defn parse
  [lines]
  (mapv (fn [line] (mapv {\. :floor \L :free} line)) lines))

(defn run
  [input a annoyance]
  (let [neighs (fn [grid [x y]]
                 (keep (fn [[dx dy]] (a grid [x y] [dx dy]))
                       [[-1 -1] [0 -1] [1 -1]
                        [-1  0]        [1  0]
                        [-1  1] [0  1] [1  1]]))
        step (fn [grid]
               (vec (map-indexed (fn [y line]
                                   (->> line
                                        (map-indexed (fn [x cell] [cell (neighs grid [x y])]))
                                        (mapv (fn [[cell neighbours]]
                                                (cond (= cell :floor) :floor
                                                      (and (= cell :free) 
                                                           (every? #{:floor :free} neighbours)) :occupied
                                                      (and (= cell :occupied)
                                                           (>= (count (filter #{:occupied} neighbours))
                                                               annoyance)) :free
                                                      :else cell)))))
                                 grid)))]
    (loop [prev input
           cur (step input)]
      (if (= prev cur)
        (->> (apply concat cur)
             (keep #{:occupied})
             count)
        (recur cur (step cur))))))

(defn part1
  [input]
  (run input
       (fn [grid [x y] [dx dy]]
         (get (get grid (+ y dy)) (+ x dx)))
       4))

(defn part2
  [input]
  (run input
       (fn a [grid [x y] [dx dy]]
         (let [g (fn [dx dy] (get (get grid (+ y dy)) (+ x dx)))]
           (loop [n 2
                  c (g dx dy)]
             (if (= :floor c)
               (recur (inc n) (g (* dx n) (* dy n)))
               c))))
       5))
