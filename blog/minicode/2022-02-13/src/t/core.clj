(ns t.core
  (:require [criterium.core :as crit])
  (:gen-class))

(defn bench
  [f]
  (->> (crit/benchmark (f) {}) :mean first))

(set! *unchecked-math* :warn-on-boxed)

(defn -main
  [& args]
  (doseq [[n mt] [["list" ()]
                  ["vector" []]
                  ["queue" clojure.lang.PersistentQueue/EMPTY]]
          size [10 100 1000]]
    (let [f #(loop [elems (range size)
                    stack-or-queue mt
                    total 0]
               (cond elems
                     (recur (next elems) (conj stack-or-queue (first elems)) total)
                     (empty? stack-or-queue)
                     total
                     :else
                     (recur nil (pop stack-or-queue) (+ (* 10 total)
                                                        (long (peek stack-or-queue))))))]
      (println (format "%s (%4d): %.2e (%d)" n size (bench f) (f))))))
