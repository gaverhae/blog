(ns t.day24)

(defn parse
  [lines]
  (->> lines
       (map (fn [l]
              (->> (re-seq #"e|se|sw|w|nw|ne" l)
                   (map {"se" [-1 0 1]
                         "sw" [0 -1 1]
                         "e" [-1 1 0]
                         "nw" [1 0 -1]
                         "ne" [0 1 -1]
                         "w" [1 -1 0]})
                   (reduce (fn [acc el]
                             [(+ (acc 0) (el 0))
                              (+ (acc 1) (el 1))
                              (+ (acc 2) (el 2))])
                           [0 0 0]))))
       (reduce (fn [acc el]
                 (update acc el (fnil not false)))
               {})
       (filter (fn [[pos black?]] black?))
       (map first)
       set))

(defn part1
  [input]
  (count input))

(defn part2
  [input]
  (let [neighbours (fn [[x y z]]
                     [[(dec x) y (inc z)]
                      [x (dec y) (inc z)]
                      [(dec x) (inc y) z]
                      [(inc x) y (dec z)]
                      [x (inc y) (dec z)]
                      [(inc x) (dec y) z]])]
    (->> (range 100)
         (reduce (fn [prev _]
                   (let [counts (java.util.HashMap.)]
                     (doseq [p prev
                             n (neighbours p)]
                       (.put counts n (inc (.getOrDefault counts n 0))))
                     (->> counts
                          (keep (fn [[pos num-black-neighbours]]
                                  ;; if zero black neighbours, does not appear
                                  (when (or (and (or (== 1 num-black-neighbours)
                                                     (== 2 num-black-neighbours))
                                                 (contains? prev pos))
                                            (and (== 2 num-black-neighbours)
                                                 (not (contains? prev pos))))
                                    pos)))
                          set)))
                 input)
         count)))

(comment
  (def in (-> (slurp "data/day24") clojure.string/split-lines parse))
  (defn go [] (part2 in))

  (go)

  )
