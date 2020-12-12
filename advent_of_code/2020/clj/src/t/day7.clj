(ns t.day7)

(defn parse
  [lines]
  (reduce (fn [acc line]
            (let [[[_ outer]] (re-seq #"^(\w+ \w+) bags contain" line)
                  inners (re-seq #",? (\d+) (\w+ \w+)" line)]
              (assoc acc outer
                     (reduce (fn [acc [_ n col]]
                               (assoc acc col (Long/parseLong n)))
                             {}
                             inners))))
          {}
          lines))

(defn part1
  [input]
  (let [inverted (reduce (fn [acc [outer inners]]
                           (reduce (fn [acc inner]
                                     (update acc inner (fnil conj #{}) outer))
                                   acc
                                   (keys inners)))
                         {}
                         input)]
    (count (loop [prev #{}
                  cur (get inverted "shiny gold")]
             (if (= prev cur) cur
               (recur cur (set (concat cur (mapcat inverted cur)))))))))

(defn part2
  [input]
  (dec ((fn rec [outer]
          (->> (input outer)
               (map (fn [[inner n]] (* n (rec inner))))
               (reduce + 1)))
        "shiny gold")))
