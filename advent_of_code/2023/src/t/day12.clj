(ns t.day12
  (:require [clojure.core.async :as async]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as s]
            [instaparse.core :as insta]
            [t.lib :as lib])
  (:import [java.util Stack Arrays]))

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
  (let [to-process (doto (Stack.)
                     (.push [(->> (re-seq #"[?#]+" symbols)
                                  (map (fn [segm]
                                         (->> segm
                                              (map {\? 0, \# 1})
                                              (into-array Long/TYPE)))))
                             pat
                             (count (re-seq #"#|\?" symbols))
                             (reduce + 0 pat)]))
        mt ^longs (make-array Long/TYPE 0)
        process (fn ! [^longs segment ss p ps num-s num-p]
                  (cond (and (> p (alength segment)) (every? zero? segment))
                        (.push to-process [ss (cons p ps) (- num-s (alength segment)) num-p])
                        (> p (alength segment))
                        nil
                        (== (aget segment 0) 0)
                        (do (! (doto (Arrays/copyOfRange segment 0 (alength segment)) (aset 0 1)) ss p ps num-s num-p)
                            (! (Arrays/copyOfRange segment 1 (alength segment)) ss p ps (dec num-s) num-p))
                        (== p (alength segment))
                        (.push to-process [ss ps (- num-s (alength segment)) (- num-p p)])
                        (== 0 (aget segment p))
                        (.push to-process [(cons (Arrays/copyOfRange segment (int (inc p)) (alength segment)) ss) ps (- num-s (inc p)) (- num-p p)])
                        (== 1 (aget segment p))
                        nil
                        :else (throw (RuntimeException. (str "Unhandled: " (pr-str [segment ss p ps num-s num-p]))))))]
    (loop [n 0]
      (if (.empty to-process)
        n
        (let [[[s & ss] [p & ps] num-s num-p]  (.pop to-process)]
          (cond (and (nil? s) (nil? p)) (recur (inc n))
                (> num-p num-s) (recur n)
                (and (nil? p) (every? (fn [segm] (every? zero? segm)) (cons s ss))) (recur (inc n))
                (nil? p) (recur n)
                (nil? s) (recur n)
                :else (do (process s ss p ps num-s num-p)
                          (recur n))))))))

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
                      shuffle
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
  [part1 sample] 21
  [part1 puzzle] 7090
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
;; 2300260284e5a 51691
;; 83517a3cb6d99 50933
;; fa62a33dc06c8 77723
;; e4c3510236acd 46092
;; 203c798e7bdcc 45295
;; b4e0cfc666a37 37906
;; df408e6b13b4f 36623
  (lib/timed (benchmark))
19051

  )
