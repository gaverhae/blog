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
  (->> input
       (filter (fn rec [[k vs]]
                 (or (contains? vs "shiny gold")
                     (some (fn [v] (rec [v (input v)])) (keys vs)))))
       count))

(defn part2
  [input]
  (dec ((fn rec [outer]
          (->> (input outer)
               (map (fn [[inner n]] (* n (rec inner))))
               (reduce + 1)))
        "shiny gold")))
