(ns t.lib
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest are is testing]]
            [hato.client :as hc]))

(defn ->long
  [s]
  (Long/parseLong s))

(defn transpose
  [s]
  (apply mapv vector s))

(defn dijkstra-search
  [initial final? generate-moves]
  (let [to-visit (java.util.PriorityQueue. 100 (fn [x y] (compare (first x) (first y))))]
    (loop [[cost state] [0 initial]
           visited #{}]
      (when (not (visited state))
        (doseq [[nxt-state nxt-cost] (generate-moves [state cost])]
          (when (not (visited nxt-state))
            (.add to-visit [nxt-cost nxt-state]))))
      (if (final? state)
        cost
        (recur (.poll to-visit)
               (conj visited state))))))

(defn a-star-search
  [initial final? generate-moves heuristic]
  (let [to-visit (java.util.PriorityQueue. 100 (fn [x y] (compare (first x) (first y))))]
    (loop [[guess cost state] [(heuristic initial) 0 initial]
           visited #{}]
      (when (not (visited state))
        (doseq [[nxt-state nxt-cost] (generate-moves [state cost])]
          (when (not (visited nxt-state))
            (.add to-visit [(+ nxt-cost (heuristic nxt-state))
                            nxt-cost nxt-state]))))
      (if (final? state)
        cost
        (recur (.poll to-visit)
               (conj visited state))))))

(defn manhattan
  [[^long x1 ^long y1] [^long x2 ^long y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defmacro check
  [& specs]
  `(deftest ~'check
     (let [~'day (-> (ns-name ~*ns*)
                     (string/replace "t.day" "")
                     (Long/parseLong))
           ~'sample (delay (-> (format "data/day%02d-sample" ~'day)
                               slurp
                               string/split-lines
                               ~'parse))
           ~'sample1 (delay (-> (format "data/day%02d-sample1" ~'day)
                                slurp
                                string/split-lines
                                ~'parse))
           ~'sample2 (delay (-> (format "data/day%02d-sample2" ~'day)
                                slurp
                                string/split-lines
                                ~'parse))
           ~'puzzle (delay
                      (let [file# (format "data/day%02d-puzzle" ~'day)]
                        (when (not (.exists (io/file file#)))
                          (spit file#
                                (-> (format "https://adventofcode.com/2023/day/%d/input"
                                            ~'day)
                                    (hc/get {:headers
                                             {"cookie" (format "session=%s"
                                                               (System/getenv "AOC_SESSION"))}})
                                    :body)))
                        (-> file#
                            slurp
                            string/split-lines
                            ~'parse)))]
       ~@(->> specs
              (partition 2)
              (map (fn [[[part input & args] expected]]
                     `(is (= ~(concat [part `(force ~input)] args) ~expected))))))))

(deftest tests
  (testing "transpose"
    (are [expected actual] (= expected actual)
         [[1 4] [2 5] [3 6]] (transpose [[1 2 3] [4 5 6]]))))
