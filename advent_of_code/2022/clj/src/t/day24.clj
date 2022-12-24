(ns t.day24
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  {:maze (->> lines
              (map-indexed
                (fn [y line]
                  (keep-indexed (fn [x c]
                                  (when (not= c \.)
                                    {[y x] [({\# :wall
                                              \> :right
                                              \< :left
                                              \^ :up
                                              \v :down} c)]}))
                                line)))
              (apply concat)
              (apply merge-with concat))
   :height (count lines)
   :width (count (first lines))
   :start [0 (.indexOf ^String (first lines) ".")]
   :end [(dec (count lines)) (.indexOf ^String (last lines) ".")]})

(defn tick-blizzards
  [{:as state :keys [maze width height]}]
  (assoc state :maze
         (->> maze
              (mapcat (fn [[[y x] ts]]
                        (->> ts
                             (map (fn [t]
                                    (case t
                                      :wall {[y x] [t]}
                                      :down {[(if (= (dec height) (inc y)) 1 (inc y)) x] [t]}
                                      :up {[(if (zero? (dec y)) (- height 2) (dec y)) x] [t]}
                                      :left {[y (if (zero? (dec x)) (- width 2) (dec x))] [t]}
                                      :right {[y (if (= (dec width) (inc x)) 1 (inc x))] [t]}))))))
              (apply merge-with concat))))

(defn generate-moves
  [[{:as state :keys [maze pos width height]} cost]]
  (let [state (tick-blizzards state)
        [y x] pos]
    (->> [[y x] [(inc y) x] [(dec y) x] [y (inc x)] [y (dec x)]]
         (filter (fn [[y x]] (and (<= 0 x (dec width))
                                  (<= 0 y (dec height)))))
         (remove (fn [pos] (get-in state [:maze pos])))
         (map (fn [new-pos]
                [(-> state (assoc :pos new-pos)) (inc cost)])))))

(defn part1
  [input]
  (lib/a-star-search
    (assoc input :pos (:start input))
    (fn [s] (= (:pos s) (:end s)))
    generate-moves
    (fn [s] (lib/manhattan (:pos s) (:end s)))))

(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 18
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
