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
        grids (loop [todo [[0 [0 0] {0 #{(:start input)}}]]
                     grids {}]
                (if (= (count grids) 13)
                  grids
                  (let [[[steps-so-far [gy gx :as grid] entry-points] & todo] todo]
                    (if (grids grid)
                      (recur todo grids)
                      (let [[grid-filled max-s-in-grid precomputed-increases grid-exits] (f entry-points)]
                        (recur (->> grid-exits
                                    (map (fn [[[dy dx] m s-min]]
                                           [(+ s-min steps-so-far) [(+ gy dy) (+ gx dx)] m]))
                                    (remove (fn [[_ grid _]] (grids grid)))
                                    (reduce conj todo)
                                    (sort-by first))
                               (assoc grids [gy gx] {:grid-filled grid-filled
                                                     :max-s max-s-in-grid
                                                     :precomputed precomputed-increases
                                                     :exits (->> grid-exits
                                                                 (map (fn [[direction points s-min]]
                                                                        [direction (+ s-min (->> points (map first) (reduce min)))]))
                                                                 (into {}))})))))))
        ;; we now have something like
        ;;                   G(-2, 0)
        ;;          G(-1,-1) g(-1, 0) G(-1, 1)
        ;; G( 0,-2) g( 0,-1) g( 0, 0) g( 0, 1) G(0, 2)
        ;;          G( 1,-1) g( 1, 0) G( 1, 1)
        ;;                   G( 2, 0)
        ;; where the small gs are unique and the big Gs repeat ad infinitum.
        ;;
        ;; So the total area will be given by however many Gs it takes to fill
        ;; up, plus special cases for the frontier.
        ;; To start, let's compute how many grids we have in each direction:
        #_#_max-cards (->> [[-1 0] [0 1] [1 0] [0 -1]]
                       (map (fn [[dy dx]]
                              [[dy dx] (-> max-steps
                                           (- (get-in grids [[0 0] :exits [dy dx]]))
                                           (- (get-in grids [[dy dx] :exits [dy dx]]))
                                           (/ (get-in grids [[(* 2 dy) (* 2 dx)] :exits [dy dx]]))
                                           long)]))
                       (into {}))
        #_#_middle-line (let [max-left (get max-cards [0 -1])
                          max-right (get max-cards [0 1])]
                      {0 (+ (get-in grids [[0 0] :precomputed 0])
                            (get-in grids [[0 -1] :precomputed (-> (get-in grids [[0 0] :exits [0 -1]]) (mod 2))])
                            (if (-> (get-in grids [[0 -1] :exits [0 -1]]) (mod 2) (= 0))
                              (* (max-cards [-1 0]) (get-in grids [[0 -1] :precomputed]))))})]
    (cond (< max-steps (-> grids (get [0 0]) :exits (->> (map (fn [[_ s]] s)) (reduce min))))
          (part1 input max-steps)
          (< max-steps (+ (-> grids (get [0 0]) :exits (->> (map (fn [[_ s]] s)) (reduce min)))
                          (->> [[0 -1] [1 0] [-1 0] [0 1]]
                               (map (fn [d] (->> (grids d) :exits (map (fn [[_ s]] s)) (reduce min)))))))
          (+ (part1 input max-steps)))))



(lib/check
  [part1 sample 6] 16
  [part1 puzzle 64] 3639
  [part2 sample 1] 2
  [part2 sample 6] 16
  [part2 sample 10] 50
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
