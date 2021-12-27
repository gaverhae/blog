(ns t.day25)

(defn parse
  [lines]
  {:max-x (dec (count (first lines)))
   :max-y (dec (count lines))
   :floor (->> lines
               (map-indexed (fn [y line]
                              (->> line
                                   (map-indexed
                                     (fn [x c]
                                       (case c
                                         \v [[y x] :down]
                                         \> [[y x] :right]
                                         \. [])))
                                   (remove #{[]}))))
               (apply concat)
               (into {}))})

(defn right
  [[y x] max-x]
  [y (if (== x max-x)
       0
       (inc x))])

(defn down
  [[y x] max-y]
  [(if (== y max-y) 0 (inc y))
   x])

(defn move
  [floor max-x max-y]
  (let [after-x (reduce
                  (fn [m [k v]]
                    (let [new-pos (right k max-x)]
                      (if (nil? (get floor new-pos))
                        (-> m (dissoc k) (assoc new-pos v))
                        m)))
                  floor
                  (->> floor (filter (fn [[_ v]] (= :right v)))))
        after-y (reduce
                  (fn [m [k v]]
                    (let [new-pos (down k max-y)]
                      (if (nil? (get after-x new-pos))
                        (-> m (dissoc k) (assoc new-pos v))
                        m)))
                  after-x
                  (->> after-x (filter (fn [[_ v]] (= :down v)))))]
    after-y))

(defn print-state
  [floor max-x max-y]
  (println)
  (doseq [y (range (inc max-y))]
    (doseq [x (range (inc max-x))]
      (print (case (get floor [y x])
               :right ">"
               :down "v"
               ".")))
    (println))
  (flush))

(defn part1
  [{:keys [floor max-y max-x]}]
  (loop [floor floor
         niter 0]
    #_(print-state floor max-x max-y)
    (let [new-floor (move floor max-x max-y)]
      (if (= new-floor floor)
        (inc niter)
        (recur new-floor (inc niter))))))

(defn part2
  [input]
  0)
