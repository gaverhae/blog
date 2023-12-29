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
            [filled? exits]
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
           done? #{}
           n 0]
      (if (empty? todo)
        (filled (mod max-steps 2))
        (let [[[steps-so-far [gy gx :as grid] entry-point] & todo] todo]
          (when (zero? (mod n 10000))
            (let [now (System/currentTimeMillis)
                  d (- now start-time)]
              (println (format "%02d:%02d:%02d[%10d]: %s"
                               (-> d (quot 1000) (quot 60) (quot 24) (mod 60))
                               (-> d (quot 1000) (quot 60) (mod 60))
                               (-> d (quot 1000) (mod 60))
                               steps-so-far
                               [:todo (count todo) :done? (count done?)]))))
          (if (done? grid)
            (recur todo filled done? (inc n))
            (let [ps (->> entry-point
                          (map (fn [[k v]] [(- k steps-so-far) v]))
                          (into {}))
                  [grid-filled grid-exits] (f ps)
                  grid-exits (->> grid-exits
                                  (map (fn [[direction m]]
                                         [direction (->> m
                                                         (map (fn [[s positions]]
                                                                [(+ steps-so-far s) positions]))
                                                         (into {}))]))
                                  (into {}))]
              (recur (->> grid-exits
                          (map (fn [[[dy dx] m]]
                                 (let [s (->> m keys (reduce min))]
                                   [s [(+ gy dy) (+ gx dx)] m])))
                          (remove (fn [[_ grid _]] (done? grid)))
                          (filter (fn [[s _ _]] (<= s max-steps)))
                          (reduce conj todo)
                          (sort-by first))
                     (reduce (fn [acc [_ s]]
                               (if (<= (+ s steps-so-far) max-steps)
                                 (update acc (mod (+ s steps-so-far) 2) inc)
                                 acc))
                             filled
                             grid-filled)
                     (conj done? grid)
                     (inc n)))))))))

(lib/check
  #_#_[part1 sample 6] 16
  #_#_[part1 puzzle 64] 3639
  #_#_[part2 sample 6] 16
  #_#_[part2 sample 10] 50
  #_#_[part2 sample 50] 1594
  #_#_[part2 sample 1] 2
  #_#_[part2 sample 10] 50
  #_#_[part2 sample 100] 6536
  #_#_[part2 sample 200] 26538
  #_#_[part2 sample 300] 59895
  #_#_[part2 sample 400] 106776
  #_#_[part2 sample 500] 167004
  #_#_[part2 sample 1000] 668697
  #_#_[part2 sample 2000] 2677337
  #_#_[part2 sample 5000] 16733044
  #_#_[part2 puzzle 100] 8829
  #_#_[part2 puzzle 200] 34889
  #_#_[part2 puzzle 400] 138314
  [part2 puzzle 1000] 862969
  [part2 puzzle 2000] 3445428
  [part2 puzzle 5000] 21527301
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
