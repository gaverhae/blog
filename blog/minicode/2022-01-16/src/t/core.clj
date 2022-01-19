(ns t.core
  (:require [taoensso.tufte :as tufte :refer (p profiled)]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(defn bitset-sieve
  [^long bound]
  (let [candidates (java.util.BitSet. bound)]
    (p :init (.set candidates 2 bound))
    (p :main-loop
       (loop [idx 2]
         (when (< idx bound)
           (when-let [next-candidate (.get candidates idx)]
             (loop [update-idx (* idx idx)]
               (when (< update-idx bound)
                 (.clear candidates update-idx)
                 (recur (+ idx update-idx)))))
           (recur (inc idx)))))
    (p :final
       (take-while pos? (iterate #(.nextSetBit candidates (inc %)) 2)))))

(defn -main
  [& args]
  (println "Waiting for profiler.")
  (read-line)
  (bitset-sieve 100000)
  #_(->> (profiled {} (bitset-sieve 100000))
       second
       deref
       pprint))
