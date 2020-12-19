(ns t.day18
  (:require [clojure.walk :as walk]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]))

(defn parse
  [lines]
  lines)

(defn part1
  [input]
  (->> input
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
             [:times a b] (* a b)
             [:plus a b] (+ a b)
             [:S e] e
             :else node)))
       (reduce +)))


(defn part2
  [input]
  (->> input
       (map (insta/parser "S = expr
                           <expr> = times | np
                           plus = np (<'+'> np)*
                           times = plus (<'*'> plus)*
                           <np> = num | par
                           <par> = w <'('> expr <')'> w
                           num = w #'\\d+' w
                           <w> = <#'\\s*'>"))
       (walk/postwalk
         (fn [node]
           (match node
                  [:num n] (Long/parseLong n)
                  [:times & r] (reduce * r)
                  [:plus & r] (reduce + r)
                  [:S e] e
                  :else node)))
            (reduce +)))
