(ns t.day16
  (:require [clojure.set :as set]
            [clojure.string :as string]))

(defn parse
  [lines]
  (let [[rules _ [_ mine] _ [_ & others]] (partition-by #{""} lines)]
    {:rules (->> rules
                 (map (fn [line]
                        (let [[_ n v1 v2 v3 v4] (re-matches #"(.+): (\d+)-(\d+) or (\d+)-(\d+)" line)
                              l (fn [s] (Long/parseLong s))]
                          [n [[(l v1) (l v2)] [(l v3) (l v4)]]])))
                 (into {}))
     :mine (mapv #(Long/parseLong %) (string/split mine #"," ))
     :others (->> others
                  (mapv (fn [line] (mapv #(Long/parseLong %) (string/split line #"," )))))}))

(defn part1
  [{:keys [rules others]}]
  (let [valid (->> rules
                   vals
                   (apply concat)
                   (mapcat (fn [[s e]] (range s (inc e))))
                   (into #{}))]
    (->> others
         (apply concat)
         (remove valid)
         (reduce +))))

(defn part2
  [{:keys [rules mine others]}]
  (let [valid (->> rules
                   vals
                   (apply concat)
                   (mapcat (fn [[s e]] (range s (inc e))))
                   (into #{}))
        valid-others (->> others
                          (remove (fn [other] (some (complement valid) other))))
        rule-sets (->> rules
                       (map (fn [[n [[v1 v2] [v3 v4]]]]
                              [n (set (concat (range v1 (inc v2))
                                              (range v3 (inc v4))))])))
        field-options (->> valid-others
                           (apply map vector)
                           (map-indexed (fn [idx values]
                                          [idx (->> rule-sets
                                                    (filter (fn [[n s]] (every? s values)))
                                                    (map first)
                                                    set)]))
                           (into {}))
        field-idx (loop [known {}
                         unknown field-options]
                    (if (empty? unknown)
                      known
                      (let [[idx f] (->> unknown (filter (fn [[_ v]] (= 1 (count v)))) first)]
                        (recur (assoc known idx (first f))
                               (reduce-kv (fn [acc k v]
                                            (assoc acc k (set/difference v f)))
                                          {}
                                          (dissoc unknown idx))))))
        departures (->> field-idx
                       (filter (fn [[idx name]] (string/starts-with? name "departure")))
                       (map first))]
    (->> departures
         (map (fn [idx] (mine idx)))
         (reduce *))))
