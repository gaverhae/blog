(ns t.day20)

(defn parse
  [lines]
  {:alg (->> lines first (mapv {\. 0 \# 1}))
   :img (->> lines (drop 2) (mapv #(mapv {\. 0 \# 1} %)))
   :default 0})

(defn neigh
  [img x y default]
  (Long/parseLong (apply str (for [y [(dec y) y (inc y)]
                                   x [(dec x) x (inc x)]]
                               (get-in img [y x] default))) 2))

(defn improve
  [{:keys [alg img default]}]
  (let [start-x -1
        start-y -1
        end-y (+ 0 (count img))
        end-x (+ 0 (count img))]
    {:alg alg
     :img (vec (for [y (range start-y (inc end-y))]
                 (vec (for [x (range start-x (inc end-x))]
                        (get alg (neigh img x y default))))))
     :default (case default
                0 (first alg)
                1 (last alg))}))

(defn part1
  [input]
  (->> (iterate improve input)
       (map (fn [x] (prn x) x))
       (drop 2)
       first
       :img
       (mapcat (fn [line] (keep #{1} line)))
       count))

(defn part2
  [input])
