(ns t.day11
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [t.lib :as lib :refer [->long]]))

(defn parse
  [lines]
  (->> lines
       (partition-by #(= % ""))
       (remove #{[""]})
       (map-indexed
         (fn [idx m]
           (let [[_ index] (re-matches #"Monkey (\d+):" (nth m 0))
                 [_ items] (re-matches #"  Starting items: (.*)" (nth m 1))
                 items (->> (string/split items #", ") (mapv ->long))
                 [_ op n] (re-matches #"  Operation: new = old (.) (.*)" (nth m 2))
                 operation (eval `(fn [~'old] (~(case op "*" * "+" +)
                                                      ~'old
                                                      ~(if (= "old" n)
                                                         'old
                                                         (->long n)))))
                 [_ div] (re-matches #"  Test: divisible by (\d+)" (nth m 3))
                 [_ t] (re-matches #"    If true: throw to monkey (\d+)" (nth m 4))
                 [_ f] (re-matches #"    If false: throw to monkey (\d+)" (nth m 5))]
             (when (not= idx (->long index))
               (throw (Exception. "Missing monkey.")))
             {:index (->long index)
              :items items
              :operation operation
              :throw-to (eval `(fn [~'w]
                                 (if (zero? (mod ~'w ~(->long div)))
                                   ~(->long t)
                                   ~(->long f))))
              :activity 0})))
       vec))

(defn part1
  [input]
  (loop [n 0
         monkeys input]
    (if (== 20 n)
      (->> monkeys
           (map :activity)
           sort
           reverse
           (take 2)
           (apply *))
      (recur (inc n)
             (loop [i 0
                    monkeys monkeys]
               (if (== i (count monkeys))
                 monkeys
                 (let [{:keys [index items operation throw-to]} (get monkeys i)]
                   (recur (inc i)
                          (reduce (fn [monkeys item]
                                    (let [w (quot (operation item) 3)
                                          t (throw-to w)]
                                      (prn [index w t])
                                      (update-in monkeys [t :items] conj w)))
                                  (-> monkeys
                                      (update-in [index :activity] + (count items))
                                      (assoc-in [index :items] []))
                                  items)))))))))

(defn part2
  [input]
  )

(lib/check
  parse
  part1 10605 117624
  #_part2
  )
