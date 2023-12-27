(ns t.day24
  (:refer-clojure :exclude [rand-int])
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.data.int-map :as i]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [[_ x y z dx dy dz] (re-find #" *(-?\d+), +(-?\d+), +(-?\d+) +@ +(-?\d+), +(-?\d+), +(-?\d+)" line)]
                (->> (mapv (comp bigint parse-long) [x y z dx dy dz])
                     ((fn [[x y z dx dy dz]]
                        [[x y z] [dx dy dz]]))))))))

(defn line-intersection
  [l1 l2]
  ;; DOES NOT COVER ALL CASES
  ;; Input never has 0 as a direction (=> (not= c1 0))
  (let [[[x1 x2] [c1 c2]] l1
        [[y1 y2] [d1 d2]] l2]
    #_(prn [:d (* d1 c2) (* d2 c1)])
    (when-not (= (* d1 c2) (* d2 c1))
      (let [b (/ (+ (* y2 c1) (* -1 x2 c1) (* -1 y1 c2) (* x1 c2))
                 (+ (* d1 c2) (* -1 d2 c1)))
            a (/ (+ y1 (* b d1) (- x1))
                 c1)]
        #_(prn [:c (->> (concat l1 l2)
                      lib/transpose
                      (map (fn [[xn cn yn dn]]
                             [(+ xn (* a cn)) (+ yn (* b dn))])))])
        (when (->> (concat l1 l2)
                   lib/transpose
                   (every? (fn [[xn cn yn dn]]
                             (= (+ xn (* a cn)) (+ yn (* b dn))))))
          [a b (->> l1 lib/transpose (map (fn [[xn cn]] (+ xn (* a cn)))))])))))

(defn part1
  [input min-c max-c]
  (let [input (->> input
                   (map (fn [[[x y z] [dx dy dz]]]
                          [[x y] [dx dy]])))]
    (->> (map vector input (iterate rest (rest input)))
         (mapcat (fn [[l1 ls]]
                   (->> ls
                        (map (fn [l2] [l1 l2])))))
         (keep (fn [[l1 l2]]
                (line-intersection l1 l2)))
         (filter (fn [[a b [x y]]]
                   (and (>= a 0)
                        (>= b 0)
                        (<= min-c x max-c)
                        (<= min-c y max-c))))
         count)))

(defn cross-product
  [[a1 a2 a3] [b1 b2 b3]]
  [(- (* a2 b3) (* a3 b2))
   (- (* a3 b1) (* a1 b3))
   (- (* a1 b2) (* a2 b1))])

(defn dot-product
  [[a1 a2 a3] [b1 b2 b3]]
  (+ (* a1 b1) (* a2 b2) (* a3 b3)))

(defn vector-minus
  [[a1 a2 a3] [b1 b2 b3]]
  [(- a1 b1) (- a2 b2) (- a3 b3)])

(defn vector-plus
  [[a1 a2 a3] [b1 b2 b3]]
  [(+ a1 b1) (+ a2 b2) (+ a3 b3)])

(defn scalar-mult
  [scalar [a1 a2 a3]]
  [(* scalar a1) (* scalar a2) (* scalar a3)])

(defn plane-from-three-points
  [p1 p2 p3]
  (let [n (cross-product (vector-minus p2 p1)
                         (vector-minus p3 p1))]
    [p1 n]))

(defn plane-line-intersection
  [line plane]
  (let [[l0 l] line
        [p0 n] plane
        denom (dot-product l n)
        nom (dot-product (vector-minus p0 l0)
                         n)]
    (if (zero? denom)
      (if (zero? nom)
        [:line line]
        [:none])
      [:point (vector-plus l0 (scalar-mult (/ nom denom) l))])))

(defn line-from-two-points
  [[a1 a2 a3] [b1 b2 b3]]
  [[a1 a2 a3] [(- b1 a1) (- b2 a2) (- b3 a3)]])

(defn is-point-on-line?
  [[[a1 a2 a3] [d1 d2 d3]] [p1 p2 p3]]
  (let [c (/ (- p1 a1) d1)]
    (and (= (+ a1 (* c d1)) p1)
         (= (+ a2 (* c d2)) p2)
         (= (+ a3 (* c d3)) p3))))

(defn vector-length
  [[x y z]]
  (Math/sqrt (+ (* 1.0 x x) (* 1.0 y y) (* 1.0 z z))))

(defn gen-search
  [lines seed]
  (let [rng (java.util.Random. seed)
        rand-int (fn [m] (bigint (* m (.nextDouble rng))))
        carousel (fn [p] (let [maxi (reduce max (map first p))
                               inverted (map (fn [[f i]] [(- maxi f) f i]) p)
                               total (reduce + (map first inverted))
                               roll (rand-int total)]
                           (loop [r roll
                                  [[f' f s] & p] inverted]
                             (if (<= r f')
                               [f s]
                               (recur (- r f') p)))))
        num-lines (count lines)
        max-time Long/MAX_VALUE
        make-solution (fn []
                        (vec (repeatedly num-lines #(rand-int max-time))))
        fitness (fn [ts]
                  (->> (map vector lines ts)
                       (map (fn [[[[x y z] [dx dy dz]] t]]
                              [t
                               (+ x (* t dx))
                               (+ y (* t dy))
                               (+ z (* y dz))]))
                       sort
                       (partition 2 1)
                       (map (fn [[[_ x1 y1 z1] [x2 y2 z2]]]
                              (let [dx (- x1 x2), dy (- y1 y2), dz (- z1 z2)]
                                (+ (* 1N dx dx) (* 1N dy dy) (* 1N dz dz)))))
                       (reduce + 0N)))
        mutate (fn [ts]
                 (let [r (rand-int 10)]
                   (case (long r)
                     0 (->> ts
                            (map (fn [t] [(rand-int max-time) t]))
                            sort
                            (map second)
                            vec)
                     1 (assoc ts (rand-int num-lines) (rand-int max-time))
                     2 (let [idx (rand-int num-lines)]
                         (update ts idx inc))
                     3 (let [idx (rand-int num-lines)]
                         (update ts idx #(max 0 (dec %))))
                     4 (let [idx (rand-int num-lines)]
                         (update ts idx * 3))
                     5 (let [idx (rand-int num-lines)]
                         (update ts idx quot 3))
                     (let [idx (rand-int num-lines)
                           t (get ts idx)
                           prev (or (->> ts sort (remove #(>= % t)) last) 0)
                           nxt (or (->> ts sort (filter #(> % t)) first) Long/MAX_VALUE)]
                       (assoc ts idx (+ (rand-int (- nxt prev)) prev))))))
        crossover (fn [t1 t2]
                    (case (int (rand-int 3))
                      0 (let [cut (rand-int num-lines)]
                          (vec (concat (take cut t1)
                                       (drop cut t2))))
                      1 (let [start (rand-int num-lines)
                              end (rand-int num-lines)
                              [start end] (sort [start end])]
                          (->> (map vector t1 t2)
                               (map-indexed (fn [idx [p1 p2]]
                                              (cond (< idx start) p1
                                                    (<= start idx end) (quot (+ p1 p2) 2)
                                                    (< end idx) p2)))
                               vec))
                      2 (->> (map (fn [p1 p2] (get [p1 p2] (rand-int 1)))
                                  t1 t2)
                             vec)))
        init-pop (->> (repeatedly 100 make-solution)
                      (map (fn [i] [(fitness i) i]))
                      sort)
        start-time (lib/now-millis)]
    (loop [population init-pop
           step 0]
      (when (zero? (rem step 1000))
        (prn [(lib/duration-since start-time) (ffirst population)
              (let [ps (->> population
                            first
                            second
                            (map (fn [[[x y z] [dx dy dz]] t]
                                   [(+ x (* t dx)) (+ y (* t dy)) (+ z (* t dz)) t])
                                 lines))
                    [lx ld] (line-from-two-points (first ps) (second ps))]
                (->> ps
                     (map (fn [[x0 y0 z0 t]]
                            (let [[x1 y1 z1] (vector-plus (scalar-mult (bigint t) ld) lx)
                                  dx (- x1 x0), dy (- y1 y0), dz (- y1 y0)]
                              (+ (* 1N dx dx) (* 1N dy dy) (* 1N dz dz)))))
                     (reduce + 0)))
              (->> population first second)]))
      (if (and (zero? (rem step 1000))
               (let [ps (->> population
                             first
                             second
                             (map (fn [[[x y z] [dx dy dz]] t]
                                    [(+ x (* t dx)) (+ y (* t dy)) (+ z (* t dz))])
                                  lines))
                     line (line-from-two-points (first ps) (second ps))]
                 (->> ps
                      (every? (fn [p] (is-point-on-line? line p))))))
        (->> population first second)
        (recur (let [survivors (concat (take 10 population)
                                       (take 3 (reverse population)))
                     new-spawns (->> (repeatedly 10 make-solution)
                                     (map (fn [i] [(fitness i) i])))
                     children (repeatedly
                                77
                                #(let [[_ parent1] (carousel population)
                                       [_ parent2] (carousel population)
                                       child (mutate (crossover parent1 parent2))]
                                   [(fitness child) child]))]
                 (sort (concat survivors new-spawns children)))
               (inc step))))))

(defn triangle-area
  [a b c]
  (let [ab (vector-minus b a)
        ac (vector-minus c a)
        dist-ab (vector-length ab)
        dist-ac (vector-length ac)
        dot (dot-product ab ac)
        cos (/ 1.0 dot dist-ab dist-ac)
        sin (Math/sqrt (- 1.0 (* cos cos)))]
    (* 0.5 dist-ab dist-ac sin)))

(defn point-at-time
  [[x dx] t]
  (vector-plus x (scalar-mult t dx)))

(defn distance-between-lines
  [l1 l2]
  (let [n (cross-product (second l1) (second l2))
        d (/ (dot-product n
                          (vector-minus (first l2) (first l1)))
             (vector-length n))]
    (if (neg? d) (- d) d)))

(defn do-the-thing
  [input start-time n]
  (->> ;; we start by taking all pairs of lines
       (map vector input (iterate rest (rest input)))
       (mapcat (fn [[l1 ls]]
                 (->> ls
                      (mapcat (fn [l2] [[l1 l2] [l2 l1]])))))
       #_((fn [x] (prn [(lib/duration-since start-time) :all-pairs (count x)]) x))
       ;; we keep just one for now
       (take 1)
       ;; then, for each pair, we make a plane by assuming (for
       ;; now) that the stone hits the first of the two hails at
       ;; t = 0; we do not know when the stone will cut through
       ;; the second hail so we form a plane with the first hail
       ;; at t=0 (so just its position) and the second hail's
       ;; trajectory (any two points)
       (map (fn [[d1 d2]]
              (plane-from-three-points (point-at-time d1 n)
                                       (point-at-time d2 0)
                                       (point-at-time d2 1))))
       #_((fn [x] (prn [(lib/duration-since start-time) :planes (count x)]) x))
       ;; we keep the planes that intersect all the lines
       (filter (fn [plane]
                 (->> input
                      (every? (fn [line] (not= [:none] (plane-line-intersection line plane)))))))
       #_((fn [x] (prn [(lib/duration-since start-time) :inters (count x)]) x))
       ;; for each plane, we keep all the intersections
       (map (fn [plane]
              (->> input (map (fn [line] (plane-line-intersection line plane))))))
       #_((fn [x] (prn [(lib/duration-since start-time) :interlines (count x)]) x))
       ;; for a single candidate plane, all the points must form a line
       (filter (fn [inters]
                 (let [points (->> inters (filter (fn [[t _]] (= t :point))) (map second))]
                   #_(prn [(lib/duration-since start-time) :interline inters (= 2 (count points))
                         (let [[p1 p2 & ps] points
                               line (line-from-two-points p1 p2)]
                           (->> ps (every? #(is-point-on-line? line %))))])
                   (if (= 2 (count points))
                     ;; two points always form a line
                     true
                     (let [[p1 p2 & ps] points
                           line (line-from-two-points p1 p2)]
                       (->> ps (every? #(is-point-on-line? line %))))))))
       #_((fn [x] (prn [(lib/duration-since start-time) :points (count x)]) x))
       ;; we replace all the points with a single line
       (map (fn [inters]
              (->> inters
                   (remove (fn [[t _]] (= t :line)))
                   (map (fn [[_ p]] p))
                   (take 2)
                   (apply line-from-two-points))))
       #_((fn [x] (prn [(lib/duration-since start-time) :lines (count x)]) x))
       ;; at this point we have the trajectory of the stone, but we
       ;; need to know its starting position; first, we compute the
       ;; intersection with each hail
       (map (fn [stone]
              [stone
               (->> input
                    (map (fn [hail] (line-intersection hail stone)))
                    ;; we care about timings, we don't care about
                    ;; where intersections happen
                    (map (fn [[a b _]] [a b])))]))
       #_((fn [x] (prn [(lib/duration-since start-time) :stones (count x)]) x))
       ;; if things work out as expected, each [a b] tuple should
       ;; have the same (- b a) value, which is the skew between
       ;; the hail timeline and the stoen timeline (i.e. the
       ;; unknown offset we introduced when we decided to set t = 0
       ;; on the first step above)
       ;; with the adjusted t = 0 we get the initial position of the stone
       (map (fn [[stone times]]
              (if (->> times
                       (map (fn [[a b]] (- b a)))
                       (apply =))
                (let [[a b] (first times)
                      offset (- b a)
                      [p d] stone]
                  (vector-plus p (scalar-mult offset d)))
                (throw (Exception. "Unexpected condition")))))
       #_((fn [x] (prn [(lib/duration-since start-time) :starts (count x)]) x))
       (map (fn [[x y z]] (+ x y z)))))

(defn part2
  [input]
  (let [start-time (lib/now-millis)]
    (println (format "%12s %14s %25s %25s %8s %20s" "Time" "Miss-by" "t1" "t2" "Dir" "Step"))
    (let [rng (java.util.Random. 0)
          rand-int (fn [m] (bigint (* m (.nextDouble rng))))
          [d1 d2] input
          miss-by (fn [t1 t2]
                    (let [p1 (point-at-time d1 t1)
                          p2 (point-at-time d2 t2)
                          d (line-from-two-points p1 p2)
                          [[t x] [dt dx]] d]
                      (->> input
                           (drop 2)
                           (take 20)
                           (map (fn [line]
                                  (when-let [[t-line t-d point] (line-intersection d line)]
                                    (let [dist (- t-line t-d)]
                                      (* dist dist)))
                                  Long/MAX_VALUE))
                           (reduce + 0N))))
          r
          (loop [iter 0
                 [t1 t2] [0 10]]
            (let [baseline (miss-by t1 t2)
                  [_ [dt1 dt2]] (->> [[-1 0] [1 0] [0 -1] [0 1]]
                                           (map (fn [[dt1 dt2]]
                                                  [(- (miss-by (+ t1 dt1) (+ t2 dt2)) baseline)
                                                   [dt1 dt2]]))
                                           sort
                                           first)]
              (when (zero? (rem iter 100))
                (print (format "%12s %14.6e %25s %25s %8s %20s"
                               (lib/duration-since start-time)
                               (* 1.0 baseline)
                               t1 t2 (pr-str [dt1 dt2]) 1))
                (flush))
              (recur (inc iter)
                     (loop [prev baseline
                            step 1N]
                       (when (zero? (rem iter 100))
                         (print (format "\r%12s %14.6e %25s %25s %8s %20s"
                                        (lib/duration-since start-time)
                                        (* 1.0 baseline)
                                        t1 t2 (pr-str [dt1 dt2]) step))
                         (flush))
                       (let [new-step (* 2 step)
                             new-baseline (miss-by (+ t1 (* new-step dt1))
                                                   (+ t2 (* new-step dt2)))]
                         (if (< new-baseline prev)
                           (recur new-baseline new-step)
                           (do (when (zero? (rem iter 100)) (println))
                               [(+ t1 (* step dt1))
                                (+ t2 (* step dt2))])))))))]
              r)
            #_(when (zero? (rem iter 100))
                (println (format "%s: %16.14e %s"
                                 (lib/duration-since start-time)
                                 tri
                                 (pr-str times))))
            #_(if (zero? tri)
                times
                (recur
                  (inc iter)
                  (let [to-adjust (rand-int 3)
                        min-step (loop [step 1N]
                                   (cond (> step (* 1N 1024 1024 1024 1024 1024 1024)) 0
                                         (< (area (update times to-adjust + step)) tri) step
                                         (< (area (update times to-adjust - step)) tri) (- step)
                                         :else (recur (* 2 step))))]
                    (loop [step min-step]
                      (if (< (area (update times to-adjust + step)) tri)
                        (recur (* 10N step))
                        (update times to-adjust + (quot step 10))))
                    #_(loop [prev-times times
                             step (* 1N 1024 1024 1024 1024 1024 1024 1024)
                             prev-size tri]
                        #_(prn [:internal prev-times (* dir step) prev-size])
                        (let [new-times (update prev-times to-adjust #(max 0N (+ (* dir step) %)))
                              new-size (area new-times)]
                          (cond (< step 1) prev-times
                                (< new-size prev-size) (recur new-times step new-size)
                                :else (recur prev-times (bigint (/ step 2)) prev-size)))))))

            #_(->> (map vector input (iterate rest (rest input)))
                   (mapcat (fn [[l1 ls]]
                             (->> ls
                                  (mapcat (fn [l2] [[l1 l2] [l2 l1]])))))
                   (map (fn [[l1 l2]]
                          [(distance-between-lines l1 l2) l1 l2]))
                   (map (fn [[d [x dx] [y dy]]]
                          (let [n (cross-product dx dy)]
                            [(/ (dot-product (cross-product dx n)
                                             (vector-minus y x))
                                (dot-product n n))
                             (/ (dot-product (cross-product dy n)
                                             (vector-minus y x))
                                (dot-product n n))])))
                   (filter (fn [[x y]] (or (integer? x) (integer? y)))))
            #_[(/ (dot-product (cross-product dx n)
                               (vector-minus y x))
                  (dot-product n n))
               (/ (dot-product (cross-product dy n)
                               (vector-minus y x))
                  (dot-product n n))]
            #_(loop [n (dec (- (long d)))]
                (when (zero? (rem n 10000))
                  (println (format "%s: %dk" (lib/duration-since start-time) (quot n 1000))))
                (let [plane (plane-from-three-points (point-at-time d1 n) p2 p3)
                      inters (->> input (map (fn [line] (plane-line-intersection line plane))))]
                  (if (and (->> inters (every? (fn [inter] (not= [:none] inter))))
                           (let [[p1 p2 & ps] (->> inters
                                                   (filter (fn [[t _]] (= t :point)))
                                                   (map second))
                                 line (line-from-two-points p1 p2)]
                             (->> ps (every? #(is-point-on-line? line %)))))
                    n
                    (recur (inc n)))))
            #_(loop [n 0]
                (when (zero? (rem n 10000))
                  (println (format "%s: %d" (lib/duration-since start-time) n)))
                (if-let [p (seq (do-the-thing input start-time n))]
                  (first p)
                  (recur (inc n)))))
          #_(let [rng (java.util.Random. 2)
                  rand-int (fn [m] (bigint (* m (.nextDouble rng))))
                  [d1 d2 d3] input
                  area (fn [[t1 t2 t3]]
                         (triangle-area (point-at-time d1 t1)
                                        (point-at-time d2 t2)
                                        (point-at-time d3 t3)))
                  start-time (lib/now-millis)
                  [t1 t2 t3]
                  (loop [iter 0
                         times [(rand-int Long/MAX_VALUE)
                                (rand-int Long/MAX_VALUE)
                                (rand-int Long/MAX_VALUE)]]
                    (let [tri (area times)]
                      (when (zero? (rem iter 100))
                        (println (format "%s: %16.14e %s"
                                         (lib/duration-since start-time)
                                         tri
                                         (pr-str times))))
                      (if (zero? tri)
                        times
                        (recur
                          (inc iter)
                          (let [to-adjust (rand-int 3)
                                min-step (loop [step 1N]
                                           (cond (> step (* 1N 1024 1024 1024 1024 1024 1024)) 0
                                                 (< (area (update times to-adjust + step)) tri) step
                                                 (< (area (update times to-adjust - step)) tri) (- step)
                                                 :else (recur (* 2 step))))]
                            (loop [step min-step]
                              (if (< (area (update times to-adjust + step)) tri)
                                (recur (* 10N step))
                                (update times to-adjust + (quot step 10))))
                            #_(loop [prev-times times
                                     step (* 1N 1024 1024 1024 1024 1024 1024 1024)
                                     prev-size tri]
                                #_(prn [:internal prev-times (* dir step) prev-size])
                                (let [new-times (update prev-times to-adjust #(max 0N (+ (* dir step) %)))
                                      new-size (area new-times)]
                                  (cond (< step 1) prev-times
                                        (< new-size prev-size) (recur new-times step new-size)
                                        :else (recur prev-times (bigint (/ step 2)) prev-size)))))))))]
              [t1 t2 t3]))

(lib/check
  #_#_[part1 sample 7 27] 2
  #_#_[part1 puzzle 200000000000000 400000000000000] 20336
  #_#_[part2 sample] 47
  [part2 puzzle] 0)
