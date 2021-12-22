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
  (let [rev (reverse input)
        x-range (->> input
                     (mapcat (fn [[_ xfrom xto]] [xfrom (inc xto)]))
                     sort
                     (partition 2 1))
        y-range (->> input
                     (mapcat (fn [[_ _ _ yfrom yto]] [yfrom (inc yto)]))
                     sort
                     (partition 2 1))
        z-range (->> input
                     (mapcat (fn [[_ _ _ _ _ zfrom zto]] [zfrom (inc zto)]))
                     sort
                     (partition 2 1))
        overlap-x (fn [[from to]]
                    (fn [[_ a b]] (not (or (< to a) (< b from)))))
        overlap-y (fn [[from to]]
                    (fn [[_ _ _ a b]] (not (or (< to a) (< b from)))))
        overlap-z (fn [[from to]]
                    (fn [[_ _ _ _ _ a b]] (not (or (< to a) (< b from)))))]
    (->> (for [[xfrom xto] x-range
               :let [rev (filter (overlap-x [xfrom (dec xto)]) rev)]
               :when (seq rev)
               :let [size (- xto xfrom)]
               [yfrom yto] y-range
               :let [rev (filter (overlap-y [yfrom (dec yto)]) rev)]
               :when (seq rev)
               :let [size (* size (- yto yfrom))]
               [zfrom zto] z-range
               :let [rev (filter (overlap-z [zfrom (dec zto)]) rev)]
               :when (seq rev)]
           (if (= :on
                  (->> rev
                       first first))
             (* size (- zto zfrom))
             0))
         (reduce +))))

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

(defn part2
  [input]
  (compute input))
