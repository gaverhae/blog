(ns t.day21
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  {:grid (->> lines
              (map (fn [line]
                     (s/replace line \S \.)))
              vec)
   :start (->> lines
               (map-indexed (fn [y line]
                              (->> line
                                   (keep-indexed (fn [x c]
                                                   (when (= c \S)
                                                     [y x]))))))
               (apply concat)
               first)})

(defn walk-one-map
  [grid]
  (let [h (-> grid count)
        w (-> grid first count)]
    (memoize
      (fn [start-ps]
        (loop [step 0
               filled? {}
               exits {}
               cur (get start-ps step)
               prev #{}]
          (if (empty? cur)
            [filled?
             (->> filled? (map second) (reduce max))
             (->> filled? (map second) (reduce (fn [acc el] (update acc (mod el 2) inc)) {0 0, 1 0}))
             (->> exits
                  (map (fn [[[dy dx] m]]
                         (let [s-min (->> m keys (reduce min))
                               m (->> m
                                      (map (fn [[s positions]]
                                             [(- s s-min) positions]))
                                      (into {}))]
                           [[dy dx] m s-min]))))]
            (let [t (->> cur
                         (mapcat (fn [[y x]]
                                   (for [[dy dx] [[-1 0] [1 0] [0 1] [0 -1]]
                                         :let [y (+ y dy)
                                               x (+ x dx)
                                               dst (get-in grid [y x] :out)]
                                         :when (and (#{\. :out} dst)
                                                    (not (prev [y x])))]
                                     [dst [y x] [dy dx]]))))
                  nxt (->> t
                           (filter (comp #{\.} first))
                           (map second)
                           (concat (get start-ps (inc step)))
                           set)
                  exits (->> t
                             (filter (comp #{:out} first))
                             (reduce (fn [acc [_ [y x] d]]
                                       (update-in acc
                                                  [d (inc step)]
                                                  (fnil conj #{})
                                                  [(mod y h) (mod x w)]))
                                     exits))
                  filled? (reduce #(assoc %1 %2 step) filled? cur)]
              (recur (inc step) filled? exits nxt cur))))))))

(defn part1
  [input max-steps]
  (let [f (walk-one-map (:grid input))
        [filled? _] (f {0 #{(:start input)}})]
    (->> filled?
         (map (fn [[_ step]] step))
         (filter (fn [step] (and (<= step max-steps)
                                 (= (mod step 2) (mod max-steps 2)))))
         count)))

(defn part2
  [input max-steps]
  (let [f (walk-one-map (:grid input))
        _ (println (format "Starting at: %s" (subs (str (java.time.LocalDateTime/now)) 0 19)))
        start-time (System/currentTimeMillis)]
    (loop [todo [[0 [0 0] {0 #{(:start input)}}]]
           filled [0 0]
           done? #{}]
      (if (empty? todo)
        (filled (mod max-steps 2))
        (let [[[steps-so-far [gy gx :as grid] entry-point] & todo] todo]
          (if (done? grid)
            (recur todo filled done?)
            (let [[grid-filled max-s-in-grid precomputed-increases grid-exits] (f entry-point)]
              (recur (->> grid-exits
                          (keep (fn [[[dy dx] m s-min]]
                                  (when (<= s-min (- max-steps steps-so-far))
                                    [(+ s-min steps-so-far) [(+ gy dy) (+ gx dx)] m])))
                          (remove (fn [[_ grid _]] (done? grid)))
                          (reduce conj todo)
                          (sort-by first))
                     (if (<= (+ steps-so-far max-s-in-grid) max-steps)
                       (let [m (mod steps-so-far 2)]
                         (-> filled
                             (update 0 + (get precomputed-increases m))
                             (update 1 + (get precomputed-increases (- 1 m)))))
                       (reduce (fn [acc [_ s]]
                                 (if (<= (+ s steps-so-far) max-steps)
                                   (update acc (mod (+ s steps-so-far) 2) inc)
                                   acc))
                               filled
                               grid-filled))
                     (conj done? grid)))))))))

(lib/check
  [part1 sample 6] 16
  [part1 puzzle 64] 3639
  [part2 sample 1] 2
  [part2 sample 6] 16
  [part2 sample 10] 50
  [part2 sample 50] 1594
  [part2 sample 100] 6536
  [part2 sample 200] 26538
  [part2 sample 300] 59895
  [part2 sample 400] 106776
  [part2 sample 500] 167004
  [part2 sample 1000] 668697
  [part2 sample 2000] 2677337
  [part2 sample 5000] 16733044
  #_#_[part2 puzzle 100] 8829
  #_#_[part2 puzzle 200] 34889
  #_#_[part2 puzzle 400] 138314
  #_#_[part2 puzzle 1000] 862969
  #_#_[part2 puzzle 2000] 3445428
  #_#_[part2 puzzle 5000] 21527301
  #_#_[part2 puzzle 26501365] 0)

(defn benchmark
  []
  (doseq [data [#'sample #'puzzle]
          n [100 300 1000 3000 10000 30000]]
    (let [start (System/currentTimeMillis)
          _ (part2 @@data n)
          end (System/currentTimeMillis)]
      (println (format "%10s %4s: %6.2fs"
                       (-> data meta :name)
                       n
                       (/ (- end start) 1000.0))))))
