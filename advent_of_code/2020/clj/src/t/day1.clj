(ns t.day1)

(defn parse
  [lines]
  (vec (map #(Long/parseLong %) lines)))

(defn part1
  [input]
  (let [len (count input)]
    (for [x (range len)
          y (range x len)
          :let [dx (input x)
                dy (input y)]
          :when (= 2020 (+ dx dy))]
      (* dx dy))))

(defn part2
  [input]
  (for [x (range (count input))
        y (range x (count input))
        z (range y (count input))
        :let [dx (input x)
              dy (input y)
              dz (input z)]
        :when (= 2020 (+ dx dy dz))]
    (* dx dy dz)))
