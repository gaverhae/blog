(ns t.day12
  (:require [clojure.core.async :as async]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Arrays]))

(defn parse
  [lines]
  (->> lines
       (map (fn [line]
              (let [[symbols bounds] (s/split line #" ")]
                [symbols
                 (->> (re-seq #"\d+" bounds)
                      (map parse-long))])))))

(defn solve-line
  [symbols pat]
  (let [process (fn [process segment n num-s num-p]
                  (let [process (fn [segment n num-s num-p] (process process segment n num-s num-p))]
                    (cond (and (> n (count segment)) (every? #{\?} segment)) [[false "" (- num-s (count segment)) num-p]]
                          (> n (count segment)) []
                          (= (first segment) \?) (concat (process (str \# (subs segment 1)) n num-s num-p)
                                                         (process (subs segment 1) n (dec num-s) num-p))
                          (= n (count segment)) [[true "" (- num-s (count segment)) (- num-p n)]]
                          (= \? (get segment n)) [[true (subs segment (inc n)) (- num-s (inc n)) (- num-p n)]]
                          (= \# (get segment n)) []
                          :else (throw (RuntimeException. (str "Unhandled: " (pr-str [segment n num-s num-p])))))))
        process (memoize process)
        process (partial process process)]
    (loop [to-process [[(->> (re-seq #"[?#]+" symbols)
                             (map (fn [s] (if (every? #{\#} s) (count s) s))))
                        pat (count (re-seq #"#|\?" symbols)) (reduce + 0 pat)]]
           n 0]
      (if (empty? to-process)
        n
        (let [[[[s & ss] [p & ps] num-s num-p] to-process] ((juxt peek pop) to-process)]
          (cond (and (integer? s) (integer? p) (== s p)) (recur (conj to-process [ss ps (- num-s s) (- num-p p)]) n)
                (integer? s) (recur to-process n)
                (and (nil? s) (nil? p)) (recur to-process (inc n))
                (> num-p num-s) (recur to-process n)
                (and (nil? p) (every? (fn [segm] (and (seqable? segm) (every? #{\?} segm))) (cons s ss))) (recur to-process (inc n))
                (nil? p) (recur to-process n)
                (nil? s) (recur to-process n)
                :else (recur (reduce (fn [acc [drop? re num-s num-p]]
                                       (conj acc [(if (seq re) (cons re ss) ss)
                                                  (if drop? ps (cons p ps))
                                                  num-s
                                                  num-p]))
                                     to-process
                                     (process s p num-s num-p))
                             n)))))))

(defn part1
  [input]
  (->> input
       (map (fn [[s p]] (solve-line s p)))
       (reduce + 0)))

(defn part2
  [input use-file?]
  (println (format "%s" (str (java.time.LocalDateTime/now))))
  (let [ins (async/chan)
        out (async/chan)
        final (async/chan)
        num-workers 7
        precomputed (if use-file?
                      (->> (slurp "day12")
                           s/split-lines
                           (map (fn [line] (s/split line #" ")))
                           (map (fn [line] (mapv parse-long line)))
                           (into {}))
                      {})
        reader (async/thread
                 (->> input
                      (map-indexed (fn [i line] [i line]))
                      reverse
                      (map (fn [[i line]]
                             (if-let [res (precomputed (inc i))]
                               (async/>!! out [(inc i) res "c" "m"])
                               (async/>!! ins [(inc i) line]))))
                      doall)
                 (async/close! ins))
        output (async/thread
                 (loop [msg (async/<!! out)
                        total 0
                        idx 0
                        workers-done 0]
                   (cond (and (= :done msg) (= (dec num-workers) workers-done))
                         (async/>!! final total)
                         (= :done msg)
                         (recur (async/<!! out) total idx (inc workers-done))
                         :else
                         (let [[n c method w] msg]
                           (println (format "%s: %4d[%4d]: %10d %s %s"
                                            (subs (str (java.time.LocalDateTime/now)) 0 19)
                                            (inc idx)
                                            n
                                            c
                                            method
                                            w))
                           (when (and use-file? (not= method "c"))
                             (spit "day12" (str n " " c "\n") :append true))
                           (recur (async/<!! out) (long (+ total c)) (inc idx) workers-done)))))
        workers (->> (range num-workers)
                     (map (fn [i]
                            (async/thread
                              (loop [line (async/<!! ins)]
                                (if-let [[n [s p]] line]
                                  (let [a (solve-line s p)
                                        b (solve-line (str s \? s) (concat p p))
                                        c (solve-line (str s \? s \? s) (concat p p p))
                                        d (quot b a)]
                                    (async/>!! out
                                               (if (= (* a d d) c)
                                                 [n (* a d d d d) "f" i]
                                                 [n (solve-line (str s \? s \? s \? s \? s)
                                                                (concat p p p p p))
                                                  "b" i]))
                                    (recur (async/<!! ins)))
                                  (async/>!! out :done))))))
                     vec)
        result (async/<!! final)]
    (async/<!! reader)
    (async/<!! output)
    (doseq [w workers] (async/<!! w))
    result))

(lib/check
  #_#_[part1 sample] 21
  #_#_[part1 puzzle] 7090
  #_#_[part2 sample false] 525152
  #_#_[part2 puzzle true] 0)

(defn benchmark
  []
  (->> @puzzle
       (take 10)
       (map (fn [[s p]] (solve-line (str s \? s \? s \? s) (concat p p p p))))
       doall)
  nil)

(comment

;; 500d98820b7f5 50058
;; without memo: 81815
  (lib/timed (benchmark))

  )
