(ns t.day16
  (:require [clojure.string :as string]))

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
  [{:keys [rules mine others]}]
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
  [input])
