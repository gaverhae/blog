(ns t.core
  (:require [criterium.core :as crit])
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn bench
  [n f]
  (let [b #(loop [n 1000]
             (when-not (== 0 n)
               (f)
               (recur (dec n))))
        time-in-seconds (->> (crit/benchmark (b) {}) :mean first)]
    (println (format "%30s: %.3e" n time-in-seconds))))

(defn -main
  [& args]
  (let [r (fn [n f]
            (let [arr (make-array Long/TYPE 10 10)]
              (bench n #(f arr))))
        v (fn [n s f]
            (let [v (into [] (range s))]
              (bench (str n " (" s ")") #(f v))))]
    (bench "no-op" (fn []))
    (r "count" count)
    (r "alength (no hint, no warn)" alength)
    (r "alength" (fn [^"[[J" arr] (alength arr)))
    (r "aget (no hint, warns)" (fn [arr] (aget arr 3)))
    (r "aget" (fn [^"[[J" arr] (aget arr 3)))
    (r "deep aget (no hint, no warn)" (fn [arr] (aget arr 3 3)))
    (r "deep aget (hint ignored)" (fn [^"[[J" arr] (aget arr 3 3)))
    (r "aset (no hint, no warn)" (fn [arr] (aset arr 3 3 1)))
    (r "aset (hint ignored)" (fn [^"[[J" arr] (aset arr 3 3 1)))
    (r "deep aset" (fn [^"[[J" arr] (aset ^longs (aget arr 3) 3 1)))
    (v "vector get" 10 (fn [v] (get v 2)))
    (v "vector get" 1000 (fn [v] (get v 2)))
    (v "vector 'set'" 10 (fn [v] (assoc v 3 1)))
    (v "vector 'set'" 1000 (fn [v] (assoc v 3 1)))
    (r "nested copy aset" (fn [^"[[J" arr]
                            (let [copy (aclone arr)
                                  to-change (aclone ^"[J" (aget arr 3))]
                              (aset to-change 3 1)
                              (aset copy 3 to-change))))))
