(ns t.day23
  (:require [t.util :refer [transpose]]))

(defn parse
  [lines]
  (->> lines
       (drop 2)
       (take 2)
       (map-indexed (fn [y line]
                      (map-indexed (fn [x c]
                                     (when-let [c ({\A 1 \B 10 \C 100 \D 1000} c)]
                                       [[(- x 1) (inc y)] c]))
                                   line)))
       (apply concat)
       (remove nil?)
       (into {})))

(def adjacent
  {[0 0] [[1 0]]
   [1 0] [[0 0] [2 0]]
   [2 0] [[1 0] [3 0] [2 1]]
   [3 0] [[2 0] [4 0]]
   [4 0] [[3 0] [5 0] [4 1]]
   [5 0] [[4 0] [6 0]]
   [6 0] [[5 0] [7 0] [6 1]]
   [7 0] [[6 0] [8 0]]
   [8 0] [[7 0] [9 0] [8 1]]
   [9 0] [[10 0] [8 0]]
   [10 0] [[9 0]]
   [2 1] [[2 0] [2 2]]
   [2 2] [[2 1]]
   [4 1] [[4 0] [4 2]]
   [4 2] [[4 1]]
   [6 1] [[6 0] [6 2]]
   [6 2] [[6 1]]
   [8 1] [[8 0] [8 2]]
   [8 2] [[8 1]]})

(def end-state
  {[2 1] 1 [2 2] 1
   [4 1] 10 [4 2] 10
   [6 1] 100 [6 2] 100
   [8 1] 1000 [8 2] 1000})

(defn possible-moves
  [amphipods]
  (->> amphipods
       (map (fn [[pos cost]]
              [pos cost
               (if (or (and (== 2 (second pos))
                            (== (end-state pos) cost))
                       (and (== 1 (second pos))
                            (== (end-state pos) cost)
                            (== (end-state pos) (amphipods [(first pos) 2]))))
                 []
                 (loop [poss [[pos 0]]
                        visited #{pos}
                        reachable []]
                   (if (empty? poss)
                     reachable
                     (let [[p cost-to-reach] (first poss)]
                       (recur (concat (rest poss)
                                      (->> (adjacent p)
                                           (remove visited)
                                           (remove amphipods)
                                           (remove (fn [adj]
                                                     (and (not= (first adj) (first pos))
                                                          (#{1 2} (second adj))
                                                          (or (and (== cost 1)
                                                                   (#{4 6 8} (first adj)))
                                                              (and (== cost 10)
                                                                   (#{2 6 8} (first adj)))
                                                              (and (== cost 100)
                                                                   (#{2 4 8} (first adj)))
                                                              (and (== cost 1000)
                                                                   (#{2 4 6} (first adj)))))))
                                           (remove (fn [adj]
                                                     (and (== 1 (second adj))
                                                          (amphipods [(first adj) 2])
                                                          (not= (amphipods [(first adj) 2]) cost))))
                                           (map (fn [adj] [adj (+ cost cost-to-reach)]))))
                              (conj visited p)
                              (if (or
                                    ;; we've already reached this one
                                    (visited p)
                                    ;; can't start in hallway, end in hallway
                                    (and (zero? (second pos))
                                         (zero? (second p)))
                                    ;; can't stop on 1 if 2 is empty
                                    (and (== 1 (second p))
                                         (nil? (-> amphipods
                                                   (dissoc pos)
                                                   (get [(first p) 2]))))
                                    ;; can't stop in front of room
                                    (#{[2 0] [4 0] [6 0] [8 0]} p))
                                reachable
                                (conj reachable [p cost-to-reach])))))))]))))

(defn print-state
  [state]
  (let [p (fn [pos]
            (get {1 "A" 10 "B" 100 "C" 1000 "D" nil "."} (get state pos)))]
    (println "#############")
    (print "#")
    (doseq [n (range 11)]
      (print (p [n 0])))
    (println "#")
    (println (format "###%s#%s#%s#%s###" (p [2 1]) (p [4 1]) (p [6 1]) (p [8 1])))
    (println (format "  #%s#%s#%s#%s#  " (p [2 2]) (p [4 2]) (p [6 2]) (p [8 2])))
    (println         "  #########  ")))

(defn abs
  [n]
  (if (neg? n) (- n) n))

(defn heuristic
  [state]
  (->> state
       (map (fn [[[x _] c]]
              (* c (abs (- x ({1 2, 10 4, 100 6, 1000 8} c))))))
       (reduce +)))

(defn part1
  [input]
  (loop [to-process [[(heuristic input) input 0]]
         visited {}
         i 0]
    (if (empty? to-process)
      :error
      (let [[[h state cost-to-reach] & to-process] to-process]
        (when (zero? (mod i 1000))
          (print-state state)
          (println [cost-to-reach h (count visited) i])
          #_(Thread/sleep 1000))
        (if (= state end-state)
          cost-to-reach
          (recur (->> (possible-moves state)
                      (mapcat (fn [[start-pos atype end-poss]]
                                (->> end-poss
                                     (map (fn [[end-pos c]]
                                            (let [cost (+ cost-to-reach c)
                                                  state (-> state
                                                            (dissoc start-pos)
                                                            (assoc end-pos atype))]
                                              [(+ cost (heuristic state))
                                               state
                                               cost]))))))
                      (concat to-process)
                      (remove (fn [[h state cost]]
                                (and (visited state)
                                     (<= (visited state) cost))))
                      (sort-by first))
                 (assoc visited state cost-to-reach)
                 (inc i)))))))

(defn part2
  [input])
