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
              (prn :ploup)
              [pos cost
               (if (or (and (== 2 (second pos))
                            (== (end-state pos) cost))
                       (and (== 1 (second pos))
                            (== (end-state pos) cost)
                            (== (end-state pos) (amphipods [(first pos) 2]))))
                 (do (prn :hey) [])
                 (loop [poss [[pos 0]]
                        visited #{pos}
                        reachable []]
                   (prn [poss visited reachable])
                   (if (empty? poss)
                     (rest reachable)
                     (let [[p cost-to-reach] (first poss)]
                       (recur (concat (rest poss)
                                      (->> (adjacent p)
                                           ((fn [x] (prn [:a x]) x))
                                           (remove visited)
                                           ((fn [x] (prn [:b x]) x))
                                           (remove amphipods)
                                           ((fn [x] (prn [:c x]) x))
                                           (remove (fn [adj]
                                                     (and (#{1 2} (second adj))
                                                          (or (and (== cost 1)
                                                                   (#{4 6 8} (first adj)))
                                                              (and (== cost 10)
                                                                   (#{2 6 8} (first adj)))
                                                              (and (== cost 100)
                                                                   (#{2 4 8} (first adj)))
                                                              (and (== cost 1000)
                                                                   (#{2 4 6} (first adj)))))))
                                           ((fn [x] (prn [:d x]) x))
                                           (remove (fn [adj]
                                                     (and (== 1 (second adj))
                                                          (amphipods [(first adj) 2])
                                                          (not= (amphipods [(first adj) 2]) cost))))
                                           ((fn [x] (prn x) x))
                                           (map (fn [adj] [adj (+ cost cost-to-reach)]))))
                              (conj visited p)
                              (if (zero? (second pos))
                                ;; started in the hallway
                                (if (or (zero? (second p)) ;; cannot remain in hallway
                                        (and (== 1 (second p))
                                             (nil? (amphipods [(first p) 2])))) ;; cannot stop at 1 if 2 is empty
                                  reachable
                                  (conj reachable [p cost-to-reach]))
                                ;; started in room,
                                (if (or (#{[2 0] [4 0] [6 0] [8 0]} p) ;;cannot stop in front of room
                                        (and (== 1 (second p))
                                             (nil? (-> amphipods
                                                       (dissoc pos)
                                                       (get [(first p) 2]))))) ;; cannot stop at 1 if 2 is empty
                                  reachable
                                  (conj reachable [p cost-to-reach]))))))))]))))

(possible-moves {[4 2] 1})

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

(defn part1
  [input]
  (loop [to-process [[input 0]]
         visited {}
         costs []
         i 0]
    (if (empty? to-process)
      (do (prn (take 5 costs)) (reduce min costs))
      (let [[[state cost-to-reach] & to-process] to-process]
        (when (zero? (mod i 1))
          (print-state state)
          (println cost-to-reach)
          (Thread/sleep 1000))
        (if (= state end-state)
          (recur to-process
                 (assoc visited state cost-to-reach)
                 (conj costs cost-to-reach)
                 (inc i))
          (recur (concat (->> (possible-moves state)
                      (mapcat (fn [[start-pos atype end-poss]]
                                (->> end-poss
                                     (map (fn [[end-pos c]]
                                            [(-> state
                                                 (dissoc start-pos)
                                                 (assoc end-pos atype))
                                             (+ cost-to-reach c)])))))
                      (remove (fn [[state cost]]
                                (and (visited state)
                                     (< (visited state) cost))))
                      #_(sort-by second))
                         to-process)
                 (assoc visited state cost-to-reach)
                 costs
                 (inc i)))))))

(defn part2
  [input])
