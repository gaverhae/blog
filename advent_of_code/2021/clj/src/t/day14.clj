(ns t.day14)

(defn parse
  [lines]
  {:start (->> lines
               first
               (partition 2 1)
               (map #(apply str %)))
   :ops (->> lines
             (drop 2)
             (map (fn [l]
                    (let [[_ k v] (re-matches #"(..) -> (.)" l)]
                      [k v])))
             (into {}))})

(defn solve
  [start ops n]
  (let [traverse' (fn [mem [a b :as pair] n]
                    (let [rec (fn [pair n] (mem mem pair n))]
                      (if (zero? n)
                        (frequencies pair)
                        (let [i (ops pair)]
                          (merge-with +
                                      (rec (str a i) (dec n))
                                      (rec (str i b) (dec n))
                                      {(first i) -1})))))
        mem-t (memoize traverse')
        traverse (partial mem-t mem-t)]
    (->> start
         (map #(traverse % n))
         (apply merge-with +
                (->> start
                     butlast
                     (map second)
                     (map #(-> {% -1}))
                     (apply merge-with + {})))
         (map val)
         sort
         ((juxt first last))
         ((fn [[small large]] (- large small))))))

(defn part1
  [{:keys [start ops]}]
  (solve start ops 10))

(defn part2
  [{:keys [start ops]}]
  (solve start ops 40))
