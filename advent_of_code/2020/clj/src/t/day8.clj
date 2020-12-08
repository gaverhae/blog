(ns t.day8)

(defn parse
  [lines]
  (mapv (fn [line]
          (let [[_ instr arg] (re-matches #"(\w+) ([+-]\d+)" line)]
            [(keyword instr) (Long/parseLong arg)]))
        lines))

(defn part1
  [input]
  (loop [pos 0
         acc 0
         visited #{}]
    (if (contains? visited pos)
      acc
      (let [[instr arg] (get input pos)]
        (case instr
          :nop (recur (inc pos) acc (conj visited pos))
          :acc (recur (inc pos) (+ acc arg) (conj visited pos))
          :jmp (recur (+ pos arg) acc (conj visited pos)))))))
