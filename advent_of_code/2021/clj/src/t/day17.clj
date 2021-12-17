(ns t.day17)

(defn parse
  [lines]
  (let [[_ x1 x2 y1 y2] (re-matches #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)"
                                    (first lines))]
    {:x [(Long/parseLong x1) (Long/parseLong x2)]
     :y [(Long/parseLong y1) (Long/parseLong y2)]}))
