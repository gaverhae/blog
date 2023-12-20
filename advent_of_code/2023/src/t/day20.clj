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
    #_(prn [:b button-pushes pulses state])
    (if (= 1000 button-pushes)
      (* (:low pulses) (:high pulses))
      (let [b (get state "broadcaster")
            [new-pulses state]
            (loop [todo (conj clojure.lang.PersistentQueue/EMPTY [:low "broadcaster" "button"])
                   state state
                   pulses {:low 1, :high 0}]
              #_(prn [:p (peek todo) (seq todo) state pulses])
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
                                   (update pulses level + (count (:outputs m)))))
                    (recur todo state pulses)))))]
        (recur (inc button-pushes) (merge-with + pulses new-pulses) state)))))

(defn part2
  [input stop-m]
  input
  (loop [button-pushes 0
         pulses {:low 0, :high 0}
         state input
         end? false]
    #_(prn [:b button-pushes pulses state])
    (when (or true
              (zero? (rem button-pushes 10000))
              (some #{1} (->> (state "qt") :state vals)))
      (pr [button-pushes
           #_(->> (state "qt") :state sort (map (fn [[k v]] v)) (map {:low 0, :high 1}))
           (->> state sort (map val)
                (filter (comp #{:conj} :type))
                (map (fn [s]
                       (if (= :flip (:type s))
                         [({:on 1, :off 0} (:state s))]
                         (->> s :state sort (map val) (map {:low 0, :high 1}))))))])
      (flush)
      (read-line))
    (if end?
      button-pushes
      (let [b (get state "broadcaster")
            [new-pulses state end?]
            (loop [todo (conj clojure.lang.PersistentQueue/EMPTY [:low "broadcaster" "button"])
                   state state
                   pulses {:low 1, :high 0}
                   end? end?]
              #_(prn [:p (peek todo) (seq todo) state pulses])
              (if (empty? todo)
                [pulses state end?]
                (let [[level target origin] (peek todo)
                      todo (pop todo)
                      m (get state target)
                      end? (or end? (and (= level :low)
                                         (= target stop-m)))]
                  (case (:type m)
                    :b (recur (->> (:outputs m)
                                   (map (fn [o] [level o target]))
                                   (reduce conj todo))
                              state
                              (update pulses level + (count (:outputs m)))
                              end?)
                    :flip (case level
                            :high (recur todo state pulses end?)
                            :low (case (:state m)
                                   :on (recur (->> (:outputs m)
                                                   (map (fn [o] [:low o target]))
                                                   (reduce conj todo))
                                              (assoc-in state [target :state] :off)
                                              (update pulses :low + (count (:outputs m)))
                                              end?)
                                   :off (recur (->> (:outputs m)
                                                    (map (fn [o] [:high o target]))
                                                    (reduce conj todo))
                                               (assoc-in state [target :state] :on)
                                               (update pulses :high + (count (:outputs m)))
                                               end?)))
                    :conj (let [new-m (assoc-in m [:state origin] level)
                                level (if (every? #{:high} (vals (:state new-m))) :low :high)]
                            (recur (->> (:outputs m)
                                        (map (fn [o] [level o target]))
                                        (reduce conj todo))
                                   (assoc state target new-m)
                                   (update pulses level + (count (:outputs m)))
                                   end?))
                    (recur todo state pulses end?)))))]
        (recur (inc button-pushes) (merge-with + pulses new-pulses) state end?)))))

(lib/check
  #_#_[part1 sample] 32000000
  #_#_[part1 sample1] 11687500
  #_#_[part1 puzzle] 879834312
  #_#_[part2 sample "inv"] 1
  #_#_[part2 sample1 "con"] 2
  [part2 puzzle "rx"] 0)
