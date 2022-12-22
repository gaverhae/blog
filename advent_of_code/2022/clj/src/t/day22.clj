(ns t.day22
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (let [[b [p]] (->> lines
                     (partition-by #{""})
                     (remove #{[""]}))
        height (count b)
        width (->> b (map count) (apply max))
        b (->> (for [j (range height)
                     :let [s (nth b j)]
                     i (range width)]
                 [[j i] (get s i \space)])
               (into {}))]
    {:board (->> b
                 (keep (fn [[[y x] c]]
                         (when (not= \space c)
                           [[(inc y) (inc x)]
                            [(loop [x (mod (inc x) width)]
                               (case (b [y x])
                                 \space (recur (mod (inc x) width))
                                 \. [(inc y) (inc x)]
                                 \# nil))
                             (loop [y (mod (inc y) height)]
                               (case (b [y x])
                                 \space (recur (mod (inc y) height))
                                 \. [(inc y) (inc x)]
                                 \# nil))
                             (loop [x (mod (dec x) width)]
                               (case (b [y x])
                                 \space (recur (mod (dec x) width))
                                 \. [(inc y) (inc x)]
                                 \# nil))
                             (loop [y (mod (dec y) height)]
                               (case (b [y x])
                                 \space (recur (mod (dec y) height))
                                 \. [(inc y) (inc x)]
                                 \# nil))]])))
                 (into {}))
     :start [1 (inc (.indexOf ^String (first lines) "."))]
     :path (->> p
                (partition-by #(Character/isDigit ^Character %))
                (map (fn [s]
                       (if (every? #(Character/isDigit ^Character %) s)
                         (->long (apply str s))
                         (apply str s)))))}))

(defn part1
  [{:keys [board start path]}]
  (loop [[y x :as pos] start
         dir (long 0)
         path path]
    (cond (empty? path)
          (+ (* 1000 y)
             (* 4 x)
             dir)
          (= "R" (first path))
          (recur pos (long (mod (inc dir) 4)) (rest path))
          (= "L" (first path))
          (recur pos (long (mod (dec dir) 4)) (rest path))
          :else
          (let [pos (loop [step (first path)
                           pos pos]
                      (if (== 0 step)
                        pos
                        (recur (dec step) (or (get-in board [pos dir]) pos))))]
            (recur pos dir (rest path))))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 6032
  [part1 puzzle] 117102
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
