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

(defn part1
  [input]
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
                    :untyped (recur todo state pulses)))))]
        (recur (inc button-pushes) (merge-with + pulses new-pulses) state)))))

(defn part2
  [input stop-m]
  (loop [button-pushes 0
         pulses {:low 0, :high 0}
         state input
         changes (->> state
                      keys
                      (map (fn [k] [k {:last-val :low
                                       :changes []}]))
                      (into {}))
         end? true]
    (if #_end?
      (= 10241 button-pushes)
      (do
        (->> changes sort (map prn) doall)
        :end)
      #_[button-pushes state]
      (let [b (get state "broadcaster")
            [new-pulses state changes end?]
            (loop [todo (conj clojure.lang.PersistentQueue/EMPTY [:low "broadcaster" "button"])
                   state state
                   pulses {:low 1, :high 0}
                   changes changes
                   end? end?]
              (if (empty? todo)
                [pulses state changes end?]
                (let [[level target origin] (peek todo)
                      todo (pop todo)
                      m (get state target)
                      changes (if (and (> 10 (count (get-in changes [origin :changes])))
                                       (not= level (get-in changes [origin :last-val])))
                                (-> changes
                                    (update-in [origin :changes] conj (inc button-pushes))
                                    (assoc-in [origin :last-val] level))
                                changes)
                      end? (and (#{"gl" "bb" "mr" "kk"} target)
                                (= level :high))]
                  (when (= target "qt")
                    (prn [:pulse button-pushes origin level]))
                  (case (:type m)
                    :b (recur (->> (:outputs m)
                                   (map (fn [o] [level o target]))
                                   (reduce conj todo))
                              state
                              (update pulses level + (count (:outputs m)))
                              changes
                              end?)
                    :flip (case level
                            :high (recur todo state pulses changes end?)
                            :low (case (:state m)
                                   :on (recur (->> (:outputs m)
                                                   (map (fn [o] [:low o target]))
                                                   (reduce conj todo))
                                              (assoc-in state [target :state] :off)
                                              (update pulses :low + (count (:outputs m)))
                                              changes
                                              end?)
                                   :off (recur (->> (:outputs m)
                                                    (map (fn [o] [:high o target]))
                                                    (reduce conj todo))
                                               (assoc-in state [target :state] :on)
                                               (update pulses :high + (count (:outputs m)))
                                               changes
                                               end?)))
                    :conj (let [new-m (assoc-in m [:state origin] level)
                                level (if (every? #{:high} (vals (:state new-m))) :low :high)]
                            (recur (->> (:outputs m)
                                        (map (fn [o] [level o target]))
                                        (reduce conj todo))
                                   (assoc state target new-m)
                                   (update pulses level + (count (:outputs m)))
                                   changes
                                   end?))
                    :untyped (recur todo state pulses changes end?)))))]
        (recur (inc button-pushes) (merge-with + pulses new-pulses) state changes end?))))
  )

(comment

  (defn f [x] (->> (iterate (fn [[n high?]] [(+ n x) (- 1 high?)]) [0 0])
                   (partition 2 1)
                   (map (fn [[[start v] [end _]]]
                          [start (dec end) v]))
                   (filter (fn [[start end v]] (= 1 v)))
                   (map (fn [[start end]] [start end]))))

  (take 3 (f 3907))
([3907 7813] [11721 15627] [19535 23441])

  (take 3 (f 3989))
([3989 7977] [11967 15955] [19945 23933])

  (take 3 (f 3967))
([3967 7933] [11901 15867] [19835 23801])

  (take 3 (f 3931))
([3931 7861] [11793 15723] [19655 23585])

(reduce lib/lcm [ 3967 3989 3931 3907])
  243037165713371
  243037165713371

; 239990823701421 --> too low!

(loop [mr (f 3907)
       kk (f 3931)
       gl (f 3939)
       bb (f 3967)
       k 0]
  (let [[mr0 mr1] mr
        [kk0 kk1] kk
        [gl0 gl1] gl
        [bb0 bb1] bb
        m (min mr0 kk0 gl0 bb0)]
    (if (< k m)
      (recur (mr kk gl bb m))
      )))




  )

(lib/check
  #_#_[part1 sample] 32000000
  #_#_[part1 sample1] 11687500
  #_#_[part1 puzzle] 879834312
  #_#_[part2 sample "inv"] 1
  #_#_[part2 sample1 "con"] 2
  [part2 puzzle "rx"] 0)
