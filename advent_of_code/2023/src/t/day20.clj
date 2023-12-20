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
         (map (fn [[n s]] [n (assoc s :inputs (get inputs n))]))
         ((fn [m] (reduce (fn [acc [k v]] (if (contains? acc k)
                                            acc
                                            (assoc acc k {:inputs v :type :untyped})))
                          (into {} m)
                          inputs)))
         (into {}))))

(defn run-machine
  [machine n-pushes]
  (loop [button-pushes 0
         pulses {:low 0, :high 0}
         state machine
         first-change {}]
    (if (= n-pushes button-pushes)
      {:signals (* (:low pulses) (:high pulses))
       :first-change first-change}
      (let [b (get state "broadcaster")
            [new-pulses state first-change]
            (loop [todo (conj clojure.lang.PersistentQueue/EMPTY [:low "broadcaster" "button"])
                   state state
                   pulses {:low 1, :high 0}
                   first-change first-change]
              (if (empty? todo)
                [pulses state first-change]
                (let [[level target origin] (peek todo)
                      todo (pop todo)
                      m (get state target)
                      first-change (if (nil? (get-in first-change [origin level]))
                                   (assoc-in first-change [origin level] (inc button-pushes))
                                   first-change)]
                  (case (:type m)
                    :b (recur (->> (:outputs m)
                                   (map (fn [o] [level o target]))
                                   (reduce conj todo))
                              state
                              (update pulses level + (count (:outputs m)))
                              first-change)
                    :flip (case level
                            :high (recur todo state pulses first-change)
                            :low (case (:state m)
                                   :on (recur (->> (:outputs m)
                                                   (map (fn [o] [:low o target]))
                                                   (reduce conj todo))
                                              (assoc-in state [target :state] :off)
                                              (update pulses :low + (count (:outputs m)))
                                              first-change)
                                   :off (recur (->> (:outputs m)
                                                    (map (fn [o] [:high o target]))
                                                    (reduce conj todo))
                                               (assoc-in state [target :state] :on)
                                               (update pulses :high + (count (:outputs m)))
                                               first-change)))
                    :conj (let [new-m (assoc-in m [:state origin] level)
                                level (if (every? #{:high} (vals (:state new-m))) :low :high)]
                            (recur (->> (:outputs m)
                                        (map (fn [o] [level o target]))
                                        (reduce conj todo))
                                   (assoc state target new-m)
                                   (update pulses level + (count (:outputs m)))
                                   first-change))
                    :untyped (recur todo state pulses first-change)))))]
        (recur (inc button-pushes) (merge-with + pulses new-pulses) state first-change)))))

(defn part1
  [input]
  (:signals (run-machine input 1000)))

(defn part2
  [input stop-m]
  (let [fc (-> (run-machine input 10000) :first-change)
        f (fn ! [m level]
            (if-let [n (get-in fc [m level])]
              n
              (let [t (get-in input [m :type])
                    level (case t
                            :untyped level
                            :conj ({:low :high, :high :low} level))]
                (->> (get-in input [m :inputs])
                     (map #(! % level))
                     (reduce *)))))]
    (f stop-m :low)))

(lib/check
  [part1 sample] 32000000
  [part1 sample1] 11687500
  [part1 puzzle] 879834312
  [part2 puzzle "rx"] 243037165713371)
