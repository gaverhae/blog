(ns t.day10
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(def parser
  (insta/parser
    "<S> = noop | add
    noop = <'noop'>
    add = <'addx '> num
    <num> = #'-?\\d+'"))

(defn parse
  [lines]
  (->> lines
       (map (comp first parser))
       (mapcat (fn [v]
                 (match v
                   [:add s] [[:noop] [:add (->long s)]]
                   _ [v])))))

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
  [part1 sample] 13140
  [part1 puzzle] 12640
  [part2 sample] ["##..##..##..##..##..##..##..##..##..##.."
                  "###...###...###...###...###...###...###."
                  "####....####....####....####....####...."
                  "#####.....#####.....#####.....#####....."
                  "######......######......######......####"
                  "#######.......#######.......#######....."]
  [part2 puzzle] ["####.#..#.###..####.#....###....##.###.."
                  "#....#..#.#..#....#.#....#..#....#.#..#."
                  "###..####.###....#..#....#..#....#.#..#."
                  "#....#..#.#..#..#...#....###.....#.###.."
                  "#....#..#.#..#.#....#....#.#..#..#.#.#.."
                  "####.#..#.###..####.####.#..#..##..#..#."])
