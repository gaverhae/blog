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

(def solve-line
  (memoize
    (fn [symbols numbers]
      (cond (and (empty? numbers) (or (empty? symbols) (every? #{\. \?} symbols))) 1
            (empty? numbers) 0
            (empty? symbols) 0
            :else (let [[max-n max-idx] (loop [[cur-n cur-idx] [(first numbers) 0]
                                               numbers (rest numbers)
                                               idx 1]
                                          (if (empty? numbers)
                                            [cur-n cur-idx]
                                            (let [[n & numbers] numbers]
                                              (recur (if (> n cur-n)
                                                       [n idx]
                                                       [cur-n cur-idx])
                                                     numbers
                                                     (inc idx)))))
                        pat-before (take max-idx numbers)
                        pat-after (drop (inc max-idx) numbers)
                        len (count symbols)
                        res (->> symbols
                                 (keep-indexed (fn [idx _]
                                                 (when (>= len (+ max-n idx))
                                                   [(subs symbols 0 idx)
                                                    (subs symbols idx (+ max-n idx))
                                                    (subs symbols (+ max-n idx))])))
                                 (filter (fn [[a b c]] (and (re-matches #"[#?]+" b)
                                                            (or (empty? a) (#{\. \?} (last a)))
                                                            (or (empty? c) (#{\. \?} (first c))))))
                                 (map (fn [[a b c]] [(if (empty? a) a (subs a 0 (dec (count a))))
                                                     (if (empty? c) c (subs c 1))]))
                                 (map (fn [[syms-before syms-after]]
                                        (* (solve-line syms-before pat-before)
                                           (solve-line syms-after pat-after))))
                                 (reduce + 0))]
                    res)))))

(defn part1
  [input]
  (->> input
       (map (fn [[s p]] (solve-line s p)))
       (reduce + 0)))

(defn part2
  [input]
  (println (format "%s" (str (java.time.LocalDateTime/now))))
  (let [ins (async/chan)
        out (async/chan)
        final (async/chan)
        num-workers 7
        reader (async/thread
                 (->> input
                      (map-indexed (fn [i line] (async/>!! ins [(inc i) line])))
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
  [part2 sample] 525152
  [part2 puzzle] 6792010726878)

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
;; ddadae4029381 19051
;; f34f96b405b4b 12785
;; 2eaf511da701b 12674
;; 529731677c78f 54177
;; b74985f669136 349278
;; bfc3032596941 39917
;; b3a6ced2b5409 38719
;; 965fc59d69828 112644
;; 0968534d264db 239
  (lib/timed (benchmark))

  )
