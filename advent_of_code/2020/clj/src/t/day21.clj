(ns t.day21
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [instaparse.core :as insta]))

(defn parse
  [lines]
  (let [parse (insta/parser "<S> = ingredients allergens
                             ingredients = word+
                             allergens = <'(contains '> word (<','> word)* <')'>
                             <word> = w #'\\w+' w
                             <w> = <#'\\s*'>")]
    (let [recipes (map (fn [line]
                         (let [[ingredients allergens] (map rest (parse line))]
                           [ingredients allergens]))
                       lines)
          possible-al (reduce (fn [acc [ings als]]
                                (reduce (fn [acc al]
                                          (if (acc al)
                                            (update acc al set/intersection (set ings))
                                            (assoc acc al (set ings))))
                                        acc
                                        als))
                              {}
                              recipes)]
      {:recipes (map first recipes)
       :allergens (loop [knowns {}
                         unknowns possible-al]
                    (if (empty? unknowns)
                      knowns
                      (let [[al ing] (->> unknowns
                                          (sort-by (fn [[k v]] (count v)))
                                          first)]
                        (recur (assoc knowns al (first ing))
                               (reduce-kv (fn [acc k v]
                                            (assoc acc k (set/difference v ing)))
                                          {}
                                          (dissoc unknowns al))))))})))

(defn part1
  [input]
  (let [als (->> input :allergens (map second) set)]
    (reduce (fn [acc recipe]
              (+ acc (count (remove als recipe))))
            0
            (:recipes input))))

(defn part2
  [input]
  (->> (:allergens input)
       (sort-by (fn [[al ing]] al))
       (map (fn [[al ing]] ing))
       (string/join ",")))
