(ns t.day11)

(defn parse
  [lines]
  [(count (first lines))
   (count lines)
   (->> (apply concat lines)
        (map {\. 0 \L 1})
        (long-array))])

(defn run
  [[^long w ^long h ^"[J" init] a ^long annoyance]
  (let [neighs-coords ^"[[I" (let [size (unchecked-int (alength init))
                                   dn (into-array [(int-array [-1 -1])
                                                   (int-array [0 -1])
                                                   (int-array [1 -1])
                                                   (int-array [-1  0])
                                                   (int-array [1  0])
                                                   (int-array [-1  1])
                                                   (int-array [0  1])
                                                   (int-array [1  1])])
                                   temp ^"[I" (make-array Integer/TYPE (unchecked-int 8))
                                   ret ^"[[I" (make-array Integer/TYPE (unchecked-int size) (unchecked-int 0))]
                               (loop [x (unchecked-int 0)
                                      y (unchecked-int 0)]
                                 (when (< (unchecked-int y) (unchecked-int h))
                                     (if (< (unchecked-int x) (unchecked-int w))
                                       (let [idx (unchecked-add-int
                                                   (unchecked-multiply-int
                                                     (unchecked-int w)
                                                     (unchecked-int y))
                                                   (unchecked-int x))]
                                         (loop [n 0
                                                v 0]
                                           (if (< (unchecked-int n) (unchecked-int 8))
                                             (let [d (aget ^"[[I" dn (unchecked-int n))]
                                               (if-let [p (a init w h x y (aget ^"[I" d (unchecked-int 0)) (aget ^"[I" d (unchecked-int 1)))]
                                                 (do (aset ^"[I" temp (unchecked-int v) (unchecked-int p))
                                                     (recur (unchecked-inc-int n) (unchecked-inc-int v)))
                                                 (recur (unchecked-inc-int n) (unchecked-int v))))
                                             (let [r (make-array Integer/TYPE (unchecked-int v))]
                                               (loop [i 0]
                                                 (when (< (unchecked-int i) (unchecked-int v))
                                                   (aset ^"[I" r (unchecked-int i)
                                                         (unchecked-int (aget ^"[I" temp (unchecked-int i))))
                                                   (recur (unchecked-inc-int i))))
                                               (aset ^"[[I" ret (unchecked-int idx) ^"[I" r))))
                                         (recur (unchecked-inc-int x) (unchecked-int y)))
                                       (recur (unchecked-int 0) (unchecked-inc-int y)))))
                               ret)
        ;_ (prn [(map seq neighs-coords)])
        step (fn b ^"[J" [^"[J" grid]
               (amap ^"[J" grid idx ret
                     (let [cell (unchecked-long (aget ^"[J" grid (unchecked-int idx)))
                           occupied-neighbours (unchecked-long (areduce ^"[I" (aget ^"[[I" neighs-coords (unchecked-int idx))
                                                                       n r (unchecked-int 0)
                                                                       (if (== (unchecked-long 2)
                                                                               (unchecked-long (aget ^"[J" grid (unchecked-int (aget ^"[I" (aget ^"[[I" neighs-coords (unchecked-int idx)) (unchecked-int n))))))
                                                                         (unchecked-inc r)
                                                                         (unchecked-long r))))]
                       (long (cond (zero? cell) (unchecked-long cell)
                                   (and (== (unchecked-long 1) (unchecked-long cell))
                                        (zero? (unchecked-long occupied-neighbours))) (unchecked-long 2)
                                   (and (== (unchecked-long 2) (unchecked-long cell))
                                        (>= (unchecked-long occupied-neighbours) (unchecked-long annoyance))) (unchecked-long 1)
                                   :else (unchecked-long cell))))))
        eq (fn c [^longs a1 ^longs a2]
             (and (== (alength a1) (alength a2))
                  (areduce a1 idx ret true
                           (if (== (aget a1 idx) (aget a2 idx))
                             ret
                             false))))]
    (loop [n 1
           prev init
           ^"[J" cur (step init)]
      (if (eq prev cur)
        (areduce cur idx ret 0
                 (if (== 2 (aget cur idx))
                   (inc ret)
                   ret))
        (recur (inc n) cur (step cur))))))

(defn part1
  [input]
  (run input
       (fn a [_ w h x y dx dy]
         (let [x (unchecked-add-int (unchecked-int x) (unchecked-int dx))
               y (unchecked-add-int (unchecked-int y) (unchecked-int dy))]
           (when (and (<= (unchecked-int 0) (unchecked-int x))
                      (< (unchecked-int x) (unchecked-int w))
                      (<= (unchecked-int 0) (unchecked-int y))
                      (< (unchecked-int y) (unchecked-int h)))
             (unchecked-add-int
               (unchecked-multiply-int
                 (unchecked-int w)
                 (unchecked-int y))
               (unchecked-int x)))))
       4))

(defn part2
  [input]
  (run input
       (fn a [grid w h x y dx dy]
         (loop [x' (int (+ x dx))
                y' (int (+ y dy))]
           (let [idx (unchecked-add-int
                       (unchecked-multiply-int
                         (unchecked-int w)
                         (unchecked-int y'))
                       (unchecked-int x'))]
             (cond (or (>= x' w) (>= y' h) (< x' 0) (< y' 0)) nil
                   (zero? (aget ^"[J" grid idx)) (recur (unchecked-add-int x' dx) (unchecked-add-int y' dy))
                   :else idx))))
       5))

(comment
  (def in (->> (slurp "data/sample11") clojure.string/split-lines))
  (def p (parse in))

  (defn go [] (doall (for [_ (range 100)] (= 37 (part1 p)))))
)
