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
                        [(keyword coord) (Long/parseLong n)]))))})

(defn part1
  [{:keys [folds dots]}]
  (->> (reductions (fn [dots fold]
                 (match fold
                   [:x n] (->> dots
                               (map (fn [[x y]]
                                      [(if (> x n) (- (* 2 n) x) x)
                                       y]))
                               set)
                   [:y n] (->> dots
                               (map (fn [[x y]]
                                      [x
                                       (if (> y n) (- (* 2 n) y) y)]))
                               set)))
               dots
               folds)
       second
       count))

(defn part2
  [{:keys [folds dots]}]
  (let [final (reduce (fn [dots fold]
                        (match fold
                          [:x n] (->> dots
                                      (map (fn [[x y]]
                                             [(if (> x n) (- (* 2 n) x) x)
                                              y]))
                                      set)
                          [:y n] (->> dots
                                      (map (fn [[x y]]
                                             [x
                                              (if (> y n) (- (* 2 n) y) y)]))
                                      set)))
                      dots
                      folds)
        [w h] (reduce (fn [[w h] [x y]] [(max w x) (max h y)]) final)]
    (->> (for [y (range (inc h))]
          (for [x (range (inc w))]
            (if (contains? final [x y])
              \# \.)))
         (map (fn [l] (apply str l))))))

