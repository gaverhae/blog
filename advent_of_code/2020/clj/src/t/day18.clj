(ns t.day18
  (:require [clojure.walk :as walk]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]))

(defn parse
  [lines]
  (->> lines
       (map (insta/parser "S = expr
                           <expr> = plus | times | np
                           plus = expr <'+'> np
                           times = expr <'*'> np
                           <np> = num | par
                           <par> = w <'('> expr <')'> w
                           num = w #'\\d+' w
                           <w> = <#'\\s*'>"))
       (walk/postwalk
         (fn [node]
           (match node
             [:num n] (Long/parseLong n)
             :times :*
             :plus :+
             [:S e] e
             :else node)))))

(defn part1
  [input]
  (->> input
       (map #(walk/postwalk
               (fn [node]
                 (match node
                        [:+ a b] (+ a b)
                        [:* a b] (* a b)
                        :else node))
               %))
       (reduce +)))

(defn part2
  [input])
