(ns t.day8)

(defn parse
  [lines]
  (mapv (fn [line]
          (let [[_ instr arg] (re-matches #"(\w+) ([+-]\d+)" line)]
            [(keyword instr) (Long/parseLong arg)]))
        lines))

(defn run
  [input]
  (loop [pos 0
         acc 0
         visited #{}]
    (cond
      (contains? visited pos) [:loop acc]
      (= pos (count input)) [:end acc]
      (> pos (count input)) [:error acc]
      :else
      (let [[instr ^long arg] (get input pos)]
         (case instr
           :nop (recur (inc pos) acc (conj visited pos))
           :acc (recur (inc pos) (unchecked-add acc arg) (conj visited pos))
           :jmp (recur (+ pos arg) acc (conj visited pos))
           [:error acc])))))

(defn part1
  [input]
  (run input))

(defn part2
  [input]
  (->> input
       (keep-indexed (fn [idx [instr arg]]
                       (when (#{:nop :jmp} instr) idx)))
       (map (fn [pos-to-change]
              (run (update input
                           pos-to-change
                           (fn [[instr arg]]
                             [({:jmp :nop, :nop :jmp} instr) arg])))))
       (filter (fn [[state acc]] (= state :end)))))
