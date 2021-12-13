(ns t.day13
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]))

(defn parse
  [lines]
  {:dots (->> lines
              (take-while #(not= "" %))
              (map (fn [l] (->> (string/split l #",")
                                (mapv #(Long/parseLong %)))))
              set)
   :folds (->> lines
               (drop-while #(not= "" %))
               rest
               (map (fn [l]
                      (let [[_ coord n] (re-matches #"fold along (.)=(.*)" l)]
                        [({"x" 0, "y" 1} coord) (Long/parseLong n)]))))})

(defn apply-folds
  [{:keys [folds dots]}]
  (let [fold (fn [n pos]
               (fn [v] (update v pos #(if (> % n) (- (* 2 n) %) %))))]
    (->> (reductions (fn [dots [dir n]]
                       (->> dots
                            (map (fold n dir))
                            set))
                     dots
                     folds))))

(defn part1
  [input]
  (->> (apply-folds input)
       second
       count))

(defn part2
  [input]
  (let [final (last (apply-folds input))
        [w h] (reduce (fn [[w h] [x y]] [(max w x) (max h y)]) final)]
    (->> (for [y (range (inc h))]
          (for [x (range (inc w))]
            (if (contains? final [x y])
              \# \.)))
         (map (fn [l] (apply str l))))))
