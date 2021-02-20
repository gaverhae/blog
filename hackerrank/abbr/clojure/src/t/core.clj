(ns t.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defmacro make-todo [i j]
  `(let [arr# (int-array 2)]
     (aset arr# 0 (int ~i))
     (aset arr# 1 (int ~j))
     arr#))

;; This is done as a macro to avoid boxing to Integer or converting to long.
(defmacro add [todo imod imat]
  `(if (.get ~'seen (unchecked-add-int ~imod (unchecked-multiply-int ~imat ~'nmod)))
     ~todo
     (conj ~todo (make-todo ~imod ~imat))))

(defn fast-abbr [^String to-modify ^String to-match]
  (let [nmod (int (count to-modify))
        nmat (int (count to-match))
        up (fn [^Character c] (Character/toUpperCase c))
        is-up? (fn [^Character c] (Character/isUpperCase c))
        seen (java.util.BitSet. (unchecked-multiply-int nmod nmat))]
    (loop [todo [(make-todo 0 0)]]
      (if (empty? todo) false
        (let [cur ^ints (peek todo)
              imod (int (aget cur 0))
              imat (int (aget cur 1))]
          (.flip seen (unchecked-add-int imod (unchecked-multiply-int imat nmod)))
          (cond
            (and (== imod nmod) (== imat nmat)) true
            (== imat nmat) (if (= (subs to-modify imod)
                                  (.toLowerCase (subs to-modify imod)))
                             true
                             (recur (pop todo)))
            (== imod nmod) (recur (pop todo))
            :else
            (let [cmod (.charAt to-modify imod)
                  cmat (.charAt to-match imat)]
              (cond
                (= cmod cmat) (recur (add (pop todo) (unchecked-inc-int imod) (unchecked-inc-int imat)))
                (is-up? cmod) (recur (pop todo))
                (not= (up cmod) cmat) (recur (add (pop todo) (unchecked-inc-int imod) imat))
                :else (recur (add (add (pop todo)
                                       (unchecked-inc-int imod) (unchecked-inc-int imat))
                                  (unchecked-inc-int imod) imat))))))))))

(defn abbr [^String to-modify ^String to-match]
  (let [s (atom #{})
        h (fn rec [^String x ^String y ^long dx ^long dy]
            (if (contains? @s [dx dy])
              false
              (do
                (swap! s conj [dx dy])
                (cond (> dy dx) false
                      (and (== dx dy) (= x y)) true
                      (and (zero? dx) (zero? dy)) true
                      (zero? dx) false
                      (zero? dy) (= x (.toLowerCase x))
                      :else
                      (let [a (first x)
                            as (subs x 1)
                            b (first y)
                            bs (subs y 1)]
                        (cond (= a b) (recur as bs (unchecked-dec dx) (unchecked-dec dy))
                              (Character/isUpperCase a) false
                              (not= (Character/toUpperCase a) b) (recur as y (unchecked-dec dx) dy)
                              (rec as bs (unchecked-dec dx) (unchecked-dec dy)) true
                              true (recur as y (unchecked-dec dx) dy)))))))]
    (h to-modify to-match (.length to-modify) (.length to-match))))

(defn -main
  [& _args]
  (let [out-file (get (System/getenv) "OUTPUT_PATH")
        num-of-items (Integer/parseInt (clojure.string/trim (read-line)))]
    (with-open [out (io/writer out-file)]
      (dotimes [i num-of-items]
        (let [to-modify (read-line)
              to-match (read-line)]
          (.write out
                  (if (fast-abbr to-modify to-match)
                    "YES\n"
                    "NO\n")))))))

; uncomment when pasting to Hackerrank
; (-main)

(comment
  (def in
    (with-open [f (io/reader "../test_data/input/13")]
      (->> f
           line-seq
           (drop 1)
           (partition 2 2)
           vec)))
  (def exp
    (with-open [f (io/reader "../test_data/output/13")]
      (->> f
           line-seq
           (map {"YES" true "NO" false})
           vec)))

  (defmacro b [e]
    `(do (dotimes [_# 10] ~e)
         (let [start# (System/currentTimeMillis)
               n# 100]
           (dotimes [_# n#] ~e)
           (/ (- (System/currentTimeMillis) start#)
              1.0
              n#))))

  (b (doseq [[mod mat] in] (abbr mod mat)))
2871.97
  (b (doseq [[mod mat] in] (fast-abbr mod mat)))
731.23
  )
