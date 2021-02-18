(ns t.day22)

(defn parse
  [lines]
  (->> lines
       (partition-by  #{""})
       (remove #{[""]})
       (map (fn [d]
              (->> d rest (map #(Long/parseLong %)))))))

(defn part1
  [[init1 init2]]
  (loop [p1 init1
         p2 init2]
    (let [win (fn [deck] (->> deck
                              reverse
                              (map-indexed vector)
                              (reduce (fn [acc [idx c]]
                                        (+ acc (* (inc idx) c)))
                                      0)))]
    (cond (empty? p1) (win p2)
          (empty? p2) (win p1)
          (> (first p1) (first p2)) (recur (concat (rest p1) [(first p1) (first p2)])
                                           (rest p2))
          (< (first p1) (first p2)) (recur (rest p1)
                                           (concat (rest p2) [(first p2) (first p1)]))))))

(defn part2
  [[init1 init2]]
  (let [q (fn q [ls] (let [ret (java.util.LinkedList.)]
                       (doseq [e ls]
                         (.add ret e))
                       ret))
        win! (fn win! [^java.util.LinkedList p1 ^java.util.LinkedList p2]
               (.add p1 (.remove p1))
               (.add p1 (.remove p2)))
        play-game (fn play-game [^java.util.LinkedList p1 ^java.util.LinkedList p2 ^java.util.Set mem]
                    (let [k (let [k (java.util.ArrayList.)
                                  k1 (java.util.ArrayList. p1)
                                  k2 (java.util.ArrayList. p2)]
                              (java.util.Collections/sort k1)
                              (java.util.Collections/sort k2)
                              (.add k k1)
                              (.add k k2)
                              k)]
                      (cond (.contains mem k) [0 p1]
                            (.isEmpty p1) [1 p2]
                            (.isEmpty p2) [0 p1]

                            true
                            (do
                              (.add mem k)
                              (cond
                                (and (> (.size p1) (.element p1))
                                     (> (.size p2) (.element p2)))
                                (let [sub1 (q (take (.element p1) (rest p1)))
                                      sub2 (q (take (.element p2) (rest p2)))
                                      [winner _] (if (> (reduce max sub1)
                                                        (reduce max sub2))
                                                   [0 nil]
                                                   (play-game sub1 sub2 (java.util.HashSet.)))]
                                  (case (int winner)
                                    0 (win! p1 p2)
                                    1 (win! p2 p1)))

                                (> (.element p1) (.element p2)) (win! p1 p2)
                                (< (.element p1) (.element p2)) (win! p2 p1))
                              (recur p1 p2 mem)))))
        [_ winner-deck] (play-game (q init1) (q init2) (java.util.HashSet.))]
    (->> winner-deck
         reverse
         (map-indexed vector)
         (reduce (fn [acc [idx c]]
                   (+ acc (* (inc idx) c)))
                 0))))

(comment

  (def in (->> (slurp "data/day22") clojure.string/split-lines parse))
  (defn go [] (part2 in))

  )
