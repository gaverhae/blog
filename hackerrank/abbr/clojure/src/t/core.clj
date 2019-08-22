(ns t.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn abbr [^String to-modify ^String to-match]
  (let [nmod (count to-modify)
        nmat (count to-match)
        up (fn [^Character c] (Character/toUpperCase c))
        is-up? (fn [^Character c] (Character/isUpperCase c))]
    (loop [seen-before? #{}
           [[imod imat] & r :as todo] [[0 0]]]
      (cond
        (empty? todo) false
        (seen-before? [imod imat]) (recur seen-before? r)
        :else
        (let [seen-now (conj seen-before? [imod imat])]
          (cond
            (and (== imod nmod) (== imat nmat)) true
            (== imat nmat) (if (= (subs to-modify imod)
                                  (.toLowerCase (subs to-modify imod)))
                             true
                             (recur seen-now r))
            (== imod nmod) (recur seen-now r)
            :else
            (let [cmod (.charAt to-modify imod)
                  cmat (.charAt to-match imat)]
              (cond
                (= cmod cmat) (recur seen-now (conj r [(inc imod) (inc imat)]))
                (is-up? cmod) (recur seen-now r)
                (not= (up cmod) cmat) (recur seen-now (conj r [(inc imod) imat]))
                :else (recur seen-now
                             (conj r [(inc imod) (inc imat)]
                                     [(inc imod) imat]))))))))))

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
