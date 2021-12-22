(ns t.day22)

(defn parse
  [lines]
  (->> lines
       (map (fn [l] (let [[_ on? x-from x-to y-from y-to z-from z-to]
                          (re-matches #"(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)"
                                      l)]
                      [({"on" :on "off" :off} on?)
                       (Long/parseLong x-from)
                       (Long/parseLong x-to)
                       (Long/parseLong y-from)
                       (Long/parseLong y-to)
                       (Long/parseLong z-from)
                       (Long/parseLong z-to)])))))

(defn overlap
  [[xf1 xt1 yf1 yt1 zf1 zt1] [xf2 xt2 yf2 yt2 zf2 zt2]]
  (if (or (< xt1 xf2) (< xt2 xf1)
          (< yt1 yf2) (< yt2 yf1)
          (< zt1 zf2) (< zt2 zf1))
    nil
    [(max xf1 xf2) (min xt1 xt2) (max yf1 yf2) (min yt1 yt2) (max zf1 zf2) (min zt1 zt2)]))

(defn compute
  [input]
  (->> (for [[xfrom xto] (->> input
                              (mapcat (fn [[_ xfrom xto]] [xfrom (inc xto)]))
                              sort
                              (partition 2 1))
             [yfrom yto] (->> input
                              (mapcat (fn [[_ _ _ yfrom yto]] [yfrom (inc yto)]))
                              sort
                              (partition 2 1))
             [zfrom zto] (->> input
                              (mapcat (fn [[_ _ _ _ _ zfrom zto]] [zfrom (inc zto)]))
                              sort
                              (partition 2 1))]
         (if (= :on
                (->> input
                     reverse
                     (filter (fn [[_ & c]] (overlap c [xfrom (dec xto) yfrom (dec yto) zfrom (dec zto)])))
                     first first))
           (* (- xto xfrom) (- yto yfrom) (- zto zfrom))
           0))
       (reduce +)))

(defn part1
  [input]
  (->> (for [[on? x-from x-to y-from y-to z-from z-to] input
             x (range x-from (inc x-to))
             :when (<= -50 x 50)
             y (range y-from (inc y-to))
             :when (<= -50 y 50)
             z (range z-from (inc z-to))
             :when (<= -50 z 50)]
         [on? [x y z]])
       (reduce (fn [acc [op k]]
                 (({:on conj :off disj} op) acc k))
               #{})
       count))

(defn corners-inside
  [[x-from1 x-to1 y-from1 y-to1 z-from1 z-to1]
   [x-from2 x-to2 y-from2 y-to2 z-from2 z-to2]]
  (for [x [x-from1 x-to1]
        y [y-from1 y-to1]
        z [z-from1 z-to1]
        :when (and (<= x-from2 x x-to2)
                   (<= y-from2 y y-to2)
                   (<= z-from2 z z-to2))]
    [x y z]))

;(defn subtract
;  [[x-from x-to y-from y-to z-from z-to :as from] sub]
;  (let [sub-in-from (corners-inside sub from)
;        from-in-sub (corners-inside from sub)]
;  (cond (and (zero? (count from-in-sub))
;             (zero? (count sub-in-from)))
;        [from]
;        (== 1 (count from-in-sub))
;        (let [[split-x split-y split-z] (first sub-in-from)
;              [lost-x lost-y lost-z] (first from-in-sub)]
;          (for [[x-from x-to] [[x-from split-x] [split-x x-to]]
;                [y-from t-to] [[y-from split-y] [split-y y-to]]
;                [z-from z-to] [[z-from split-z] [split-z z-to]]]
;
;


(defn part2
  [input]
  (compute input)
  #_(->> input
       (reduce (fn [acc [on? x-from x-to y-from y-to z-from z-to & r]]
                 (case on?
                   :on
                   :off))
               [])))
