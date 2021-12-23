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
                   #_(prn [pos p visited
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
                                    (#{[2 0] [4 0] [6 0] [8 0]} p) ])
                       #_(prn [[(zero? (second pos))
                              (or (zero? (second p))
                                  (and (== 1 (second p))
                                       (nil? (amphipods [(first p) 2]))))
                              (or (#{[2 0] [4 0] [6 0] [8 0]} p)
                                  (and (== 1 (second p))
                                       (nil? (-> amphipods
                                                 (dissoc pos)
                                                 (get [(first p) 2])))))]])
                       (recur (concat (rest poss)
                                      (->> (adjacent p)
                                           #_((fn [x] (prn [:a x]) x))
                                           (remove visited)
                                           #_((fn [x] (prn [:b x]) x))
                                           (remove amphipods)
                                           #_((fn [x] (prn [:c x]) x))
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
                                           #_((fn [x] (prn [:d x]) x))
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

(comment
(possible-moves
  {[5 0] 1000
   [7 0] 1000
   [9 0] 1})

(possible-moves
  end-state)

(possible-moves
  {[2 2] 1 [4 1] 10 [4 2] 10 [6 1] 100 [6 2] 100 [8 1] 1000 [8 2] 1000 [9 0] 1})
([[2 2] 1 []] [[4 1] 10 []] [[4 2] 10 []] [[6 1] 100 []] [[6 2] 100 []] [[8 1] 1000 []] [[8 2] 1000 []] [[9 0] 1 [[[2 1] 8]]])

(possible-moves
  {[2 2] 1 [4 1] 10 [4 2] 10 [5 0] 1000 [6 1] 100 [6 2] 100 [7 0] 1000 [9 0] 1})
([[2 2] 1 []] [[4 1] 10 []] [[4 2] 10 []] [[5 0] 1000 []] [[6 1] 100 []] [[6 2] 100 []] [[7 0] 1000 [[[8 2] 3000]]] [[9 0] 1 []])

(possible-moves
  {[2 1] 10 [2 2] 1
   [4 1] 100 [4 2] 1000
   [6 1] 10 [6 2] 100
   [8 1] 1000 [8 2] 1})
([[2 1] 10 [[[2 1] 0] [[1 0] 20] [[3 0] 20] [[0 0] 30] [[5 0] 40] [[7 0] 60] [[9 0] 80] [[10 0] 90]]] [[2 2] 1 []] [[4 1] 100 [[[4 1] 0] [[3 0] 200] [[5 0] 200] [[1 0] 400] [[7 0] 400] [[0 0] 500] [[9 0] 600] [[10 0] 700]]] [[4 2] 1000 [[[4 2] 0]]] [[6 1] 10 [[[6 1] 0] [[5 0] 20] [[7 0] 20] [[3 0] 40] [[9 0] 40] [[10 0] 50] [[1 0] 60] [[0 0] 70]]] [[6 2] 100 []] [[8 1] 1000 [[[8 1] 0] [[7 0] 2000] [[9 0] 2000] [[10 0] 3000] [[5 0] 4000] [[3 0] 6000] [[1 0] 8000] [[0 0] 9000]]] [[8 2] 1 [[[8 2] 0]]])
)

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

(defn value
  [state]
  (+ (if (= (state [8 2]) 1000)
       1000 0)
     (if (and (= (state [8 2]) 1000)
              (= (state [8 1]) 1000))
       1000 0)
    (if (= (state [6 2]) 100)
      1000 0)
     (if (and (= (state [6 2]) 100)
              (= (state [6 1]) 100))
       1000 0)
    (if (= (state [4 2]) 10)
      1000 0)
     (if (and (= (state [4 2]) 10)
              (= (state [4 1]) 10))
       1000 0)
    (if (= (state [2 2]) 1)
      1000 0)
     (if (and (= (state [2 2]) 1)
              (= (state [2 1]) 1))
       1000 0)))

(defn part1
  [input]
  (loop [to-process [[input 0]]
         visited {}
         costs []
         i 0]
    (if (empty? to-process)
      (do (prn (take 5 costs)) (reduce min costs))
      (let [[[state cost-to-reach] & to-process] to-process]
        (when (zero? (mod i 1000))
          (print-state state)
          (println [cost-to-reach (value state) costs (count visited) i])
          #_(Thread/sleep 1000))
        (if (= state end-state)
          (recur to-process
                 (assoc visited state cost-to-reach)
                 (conj costs cost-to-reach)
                 (inc i))
          (recur (->> (possible-moves state)
                      (mapcat (fn [[start-pos atype end-poss]]
                                (->> end-poss
                                     (map (fn [[end-pos c]]
                                            [(-> state
                                                 (dissoc start-pos)
                                                 (assoc end-pos atype))
                                             (+ cost-to-reach c)])))))
                      (concat to-process)
                      (remove (fn [[state cost]]
                                (and (visited state)
                                     (< (visited state) cost))))
                      (sort-by #(get % 1) #_(fn [[state cost-to-reach]]
                                 (- cost-to-reach
                                    (value state)))))
                 (assoc visited state cost-to-reach)
                 costs
                 (inc i)))))))

(defn part2
  [input])
