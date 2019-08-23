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
     (conj ~todo (make-todo-arr ~imod ~imat))))

(defn abbr [^String to-modify ^String to-match]
  (let [nmod (int (count to-modify))
        nmat (int (count to-match))
        up (fn [^Character c] (Character/toUpperCase c))
        is-up? (fn [^Character c] (Character/isUpperCase c))
        seen (java.util.BitSet. (unchecked-multiply-int nmod nmat))]
    (loop [todo [(make-todo-arr 0 0)]]
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

(defn -main
  [& _args]
  (let [out-file (get (System/getenv) "OUTPUT_PATH")
        num-of-items (Integer/parseInt (clojure.string/trim (read-line)))]
    (with-open [out (io/writer out-file)]
      (dotimes [i num-of-items]
        (let [to-modify (read-line)
              to-match (read-line)]
          (.write out
                  (if (abbr to-modify to-match)
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
  (defmacro timed [expr]
    `(let [start# (System/nanoTime)
           _# ~expr]
       (format "%9.6f" (/ (- (System/nanoTime) start#) 1000000.0))))


  (mapv (fn [_] (timed (doseq [[mod mat] in] (abbr mod mat))))
        (range 100))
)
