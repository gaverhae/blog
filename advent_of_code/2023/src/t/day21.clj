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
             (let [entry-dir? (->> start-ps
                                   (mapcat val)
                                   (remove #{[0 0] [(dec h) 0] [0 (dec w)] [(dec h) (dec w)]})
                                   (mapcat (fn [[y x]]
                                             (->> [(when (= y 0) [-1 0])
                                                   (when (= x 0) [0 -1])
                                                   (when (= y (dec h)) [1 0])
                                                   (when (= x (dec w)) [0 1])]
                                                  (keep identity))))
                                   (into #{}))]
               (->> exits
                    (map (fn [[[dy dx] m]]
                           (let [s-min (->> m keys (reduce min))
                                 m (->> m
                                        (map (fn [[s positions]]
                                               [(- s s-min) positions]))
                                        (into {}))]
                             [[dy dx] m s-min])))
                    (remove (fn [[d m s-min]] (entry-dir? d)))))]
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
        start-time (System/currentTimeMillis)]
    (println)
    (loop [todo {{0 #{(:start input)}} {0 #{[0 0]}}}
           filled [0 0]]
      (when-not (empty? todo)
        (print "\r")
        (pr [(lib/duration-since start-time)
             (format "%05.2f" (/ (->> todo first val keys (reduce min)) max-steps 0.01))])
        (flush))
      (if (empty? todo)
        (filled (mod max-steps 2))
        (let [todo' (->> todo
                         (mapcat (fn [[entry-points steps-to-grids]]
                                   (let [[grid-filled max-s-in-grid precomputed-increases grid-exits] (f entry-points)]
                                     (->> steps-to-grids
                                          (mapcat (fn [[steps-so-far grids]]
                                                    (->> grids
                                                         (mapcat (fn [[gy gx :as grid]]
                                                                   (->> grid-exits
                                                                        (keep (fn [[[dy dx] m s-min]]
                                                                                (when (<= s-min (- max-steps steps-so-far))
                                                                                  [(+ s-min steps-so-far) [(+ gy dy) (+ gx dx)] m])))))))))))))
                         (reduce (fn [acc [steps-so-far grid entry-points]]
                                   (update acc grid (fn [[s0 ep0 :as e] [s1 ep1]]
                                                      (if e
                                                        [(min s0 s1) (merge-with set/union ep0 ep1)]
                                                        [s1 ep1]))
                                           [steps-so-far entry-points]))
                                 {})
                         (reduce (fn [acc [g [s e]]]
                                   (update-in acc [e s] (fnil conj #{}) g))
                                 {}))
              filled' (->> todo
                           (mapcat (fn [[entry-points steps-to-grids]]
                                     (let [[grid-filled max-s-in-grid precomputed-increases grid-exits] (f entry-points)]
                                       (->> steps-to-grids
                                            (map (fn [[steps-so-far grids]]
                                                   (let [c (count grids)
                                                         updates (if (<= (+ steps-so-far max-s-in-grid) max-steps)
                                                                   (let [m (mod steps-so-far 2)]
                                                                     (-> [0 0]
                                                                         (update 0 + (* c (get precomputed-increases m)))
                                                                         (update 1 + (* c (get precomputed-increases (- 1 m))))))
                                                                   (reduce (fn [acc [_ s]]
                                                                             (if (<= (+ s steps-so-far) max-steps)
                                                                               (update acc (mod (+ s steps-so-far) 2) + c)
                                                                               acc))
                                                                           [0 0]
                                                                           grid-filled))]
                                                     updates)))))))
                           (reduce (fn [[y0 x0] [y1 x1]]
                                     [(+ y0 y1) (+ x0 x1)])
                                   filled))]
          (recur todo' filled'))))))

(lib/check
  #_#_[part1 sample 6] 16
  #_#_[part1 puzzle 64] 3639
  #_#_[part2 sample 1] 2
  #_#_[part2 sample 6] 16
  #_#_[part2 sample 10] 50
  #_#_[part2 sample 50] 1594
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
  #_#_[part2 puzzle 1000] 862969
  #_#_[part2 puzzle 2000] 3445428
  #_#_[part2 puzzle 5000] 21527301
  [part2 puzzle 26501365] 0)

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
