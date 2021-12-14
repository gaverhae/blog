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

(defn part1
  [{:keys [start ops]}]
  (let [final (nth (iterate (fn [chain]
                              (mapcat (fn [[a b :as pair]]
                                        (let [insert (ops pair)]
                                          [(str a insert) (str insert b)]))
                                      chain))
                            start)
                   10)]
    (->> (cons (ffirst final) (map second final))
         frequencies
         (map val)
         sort
         ((juxt first last))
         ((fn [[small large]] (- large small))))))

(defn part2
  [{:keys [start ops]}]
  (let [traverse' (fn [mem]
                    (fn [[a b :as pair] n]
                      (let [rec (mem mem)]
                        (if (zero? n)
                          (frequencies pair)
                          (let [i (ops pair)]
                            (merge-with +
                                        (rec (str a i) (dec n))
                                        (rec (str i b) (dec n))
                                        {(first i) -1}))))))
        traverse (traverse' traverse')]
    (->> start
         (map #(traverse % 21))
         (apply merge-with +
                (->> start
                     butlast
                     (map second)
                     (map #(-> [% -1]))
                     (into {})))))

  #_(let [final (second (nth (iterate (fn [[n chain]]
                              (prn n)
                              [(inc n) (vec (mapcat (fn [[a b :as pair]]
                                                      (let [insert (ops pair)]
                                                        [(str a insert) (str insert b)]))
                                                    chain))])
                            [0 start])
                   40))]
    (->> (cons (ffirst final) (map second final))
         frequencies
         (map val)
         sort
         ((juxt first last))
         ((fn [[small large]] (- large small))))))
