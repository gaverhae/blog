(ns t.day20
  (:require [clojure.core.async :as async]
            [clojure.core.match :refer [match]]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(def parser
  (insta/parser
    "
    <S> = type name <' -> '> output
    type = '&' | '%' | ''
    <name> = #'[a-z]+'
    <output> = name (<', '> name)*
    "))

(defn parse
  [lines]
  (let [m (->> lines
               (map parser)
               (map (fn [[[_ t] n & outs]]
                      [n {:type ({"&" :conj, "%" :flip, nil :b} t)
                          :outputs outs}]))
               (into {}))
        inputs (->> m
                    (mapcat (fn [[k v]]
                              (map (fn [o] [o k])
                                   (:outputs v))))
                    (reduce (fn [acc [k v]]
                              (update acc k (fnil conj []) v))
                            {}))]
    (->> m
         (map (fn [[n s]]
                [n (if (= :flip (:type s))
                     (assoc s :state :off)
                     s)]))
         (map (fn [[n s]]
                [n (if (= :conj (:type s))
                     (assoc s :state (->> (get inputs n)
                                          (map (fn [o] [o :low]))
                                          (into {})))
                     s)]))
         (into {}))))

(defn part1
  [input]
  input
  (loop [button-pushes 0
         pulses {:low 0, :high 0}
         state input]
    (prn [:b button-pushes pulses state])
    (if (= 1000 button-pushes)
      (* (:low pulses) (:high pulses))
      (let [b (get state "broadcaster")
            [new-pulses state]
            (loop [todo (conj clojure.lang.PersistentQueue/EMPTY [:low "broadcaster" "button"])
                   state state
                   pulses {:low 1, :high 0}]
              (prn [:p (peek todo) (seq todo) state pulses])
              (if (empty? todo)
                [pulses state]
                (let [[level target origin] (peek todo)
                      todo (pop todo)
                      m (get state target)]
                  (case (:type m)
                    :b (recur (->> (:outputs m)
                                   (map (fn [o] [level o target]))
                                   (reduce conj todo))
                              state
                              (update pulses level + (count (:outputs m))))
                    :flip (case level
                            :high (recur todo state pulses)
                            :low (case (:state m)
                                   :on (recur (->> (:outputs m)
                                                   (map (fn [o] [:low o target]))
                                                   (reduce conj todo))
                                              (assoc-in state [target :state] :off)
                                              (update pulses :low + (count (:outputs m))))
                                   :off (recur (->> (:outputs m)
                                                    (map (fn [o] [:high o target]))
                                                    (reduce conj todo))
                                               (assoc-in state [target :state] :on)
                                               (update pulses :high + (count (:outputs m))))))
                    :conj (let [new-m (assoc-in m [:state origin] level)
                                level (if (every? #{:high} (vals (:state new-m))) :low :high)]
                            (recur (->> (:outputs m)
                                        (map (fn [o] [level o target]))
                                        (reduce conj todo))
                                   (assoc state target new-m)
                                   (update pulses level + (count (:outputs m)))))))))]
        (recur (inc button-pushes) (merge-with + pulses new-pulses) state)))))



(defn part2
  [input]
  input)

(lib/check
  [part1 sample] 32000000
  #_#_[part1 sample1] 11687500
  #_#_[part1 puzzle] 0
  #_#_[part2 sample] 0
  #_#_[part2 puzzle] 0)
