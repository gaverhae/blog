(ns t.day10
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (mapcat (fn [line]
                 (or (when-let [[_ n] (re-matches #"addx (-?\d+)" line)]
                       [[:noop] [:add (->long n)]])
                     (when-let [[_] (re-matches #"noop" line)]
                       [[:noop]])
                     (throw (Exception.)))))))

(defn part1
  [input]
  (->> input
       (reductions (fn [state op]
                     (match op
                       [:noop] (-> state
                                   (update :pc inc))
                       [:add x] (-> state
                                    (update :pc inc)
                                    (update :x + x))))
                   {:pc 1 :x 1})
       (filter (fn [state]
                 (zero? (mod (+ 20 (:pc state)) 40))))
       (map (fn [state]
              (* (:pc state) (:x state))))
       (reduce + 0)))

(defn part2
  [input]
  (->> input
       (reductions (fn [state op]
                     (match op
                       [:noop] (-> state
                                   (update :pc inc))
                       [:add x] (-> state
                                    (update :pc inc)
                                    (update :x + x))))
                   {:pc 1 :x 1})
       (map (fn [{:keys [pc x]}]
              (let [pos (mod (dec pc) 40)]
              (if (<= (dec pos) x (inc pos))
                "#"
                "."))))
       (partition 40)
       (map #(apply str %))))

(lib/check
  parse
  part1 13140 12640
  part2
  ["##..##..##..##..##..##..##..##..##..##.."
   "###...###...###...###...###...###...###."
   "####....####....####....####....####...."
   "#####.....#####.....#####.....#####....."
   "######......######......######......####"
   "#######.......#######.......#######....."]
  ["####.#..#.###..####.#....###....##.###.."
   "#....#..#.#..#....#.#....#..#....#.#..#."
   "###..####.###....#..#....#..#....#.#..#."
   "#....#..#.#..#..#...#....###.....#.###.."
   "#....#..#.#..#.#....#....#.#..#..#.#.#.."
   "####.#..#.###..####.####.#..#..##..#..#."])
