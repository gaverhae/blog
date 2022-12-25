(ns t.day24
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  {:blizzards (->> lines
                   (map-indexed
                     (fn [y line]
                       (->> line
                            (keep-indexed
                              (fn [x c]
                                (case c
                                  \. nil
                                  \# nil
                                  \> [:right [y x]]
                                  \< [:left [y x]]
                                  \^ [:up [y x]]
                                  \v [:down [y x]]))))))
                   (apply concat)
                   (reduce (fn [acc [dir pos]]
                             (update acc dir (fnil conj []) pos))
                           {}))
   :height (count lines)
   :width (count (first lines))
   :start [0 (.indexOf ^String (first lines) ".")]
   :end [(dec (count lines)) (.indexOf ^String (last lines) ".")]})

(defn collides?
  [blizzards [y x] t width height]
  (let [left (->> blizzards :left (filter (fn [[yb _]] (= yb y))))
        right (->> blizzards :right (filter (fn [[yb _]] (= yb y))))
        up (->> blizzards :up (filter (fn [[_ xb]] (= xb x))))
        down (->> blizzards :down (filter (fn [[_ xb]] (= xb x))))]
    (or (some (fn [[_ xb]] (= x (inc (mod (+ (dec xb) (- t)) (- width 2))))) left)
        (some (fn [[_ xb]] (= x (inc (mod (+ (dec xb)    t ) (- width 2))))) right)
        (some (fn [[yb _]] (= y (inc (mod (+ (dec yb) (- t)) (- height 2))))) up)
        (some (fn [[yb _]] (= y (inc (mod (+ (dec yb)    t ) (- height 2))))) down))))

(defn generate-moves
  [{:keys [blizzards width height start end]}]
  (fn [[[y x cost] _]]
    (->> [[y x] [(inc y) x] [(dec y) x] [y (inc x)] [y (dec x)]]
         (filter (fn [[y x]] (or (and (<= 1 x (- width 2))
                                      (<= 1 y (- height 2)))
                                 (= start [y x])
                                 (= end [y x]))))
         (remove (fn [pos] (collides? blizzards pos (inc cost) width height)))
         (map (fn [[y x]] [[y x (inc cost)] (inc cost)])))))

(defn part1
  [input]
  (lib/a-star-search
    (conj (:start input) 0)
    (let [end (:end input)] (fn [[y x _]] (= [y x] end)))
    (generate-moves input)
    (let [end (:end input)] (fn [[y x _]] (lib/manhattan [y x] end)))))

(defn part2
  [input]
  (let [trip1 (lib/a-star-search
                (conj (:start input) 0)
                (let [end (:end input)] (fn [[y x _]] (= [y x] end)))
                (generate-moves input)
                (let [end (:end input)] (fn [[y x _]] (lib/manhattan [y x] end))))
        trip2 (lib/a-star-search
                (conj (:end input) trip1)
                (let [end (:start input)] (fn [[y x _]] (= [y x] end)))
                (generate-moves input)
                (let [end (:start input)] (fn [[y x _]] (lib/manhattan [y x] end))))
        trip3 (lib/a-star-search
                (conj (:start input) trip2)
                (let [end (:end input)] (fn [[y x _]] (= [y x] end)))
                (generate-moves input)
                (let [end (:end input)] (fn [[y x _]] (lib/manhattan [y x] end))))]
    trip3))

(lib/check
  #_#_[part1 sample] 18
  #_#_[part1 puzzle] 299
  #_#_[part2 sample] 54
  #_#_[part2 puzzle] 899)
