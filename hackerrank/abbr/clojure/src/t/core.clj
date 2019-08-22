(ns t.core
  (:require [clojure.java.io :as io])
  (:gen-class))


(defn abbr [^String to-modify ^String to-match]
  (let [h (fn [^String i ^String j rec]
            (cond
              (= i j) true
              (empty? i) (empty? j)
              (empty? j) (= i (.toLowerCase i))
              (= (first i) (first j)) (rec (subs i 1) (subs j 1) rec)
              (Character/isUpperCase (first i)) false
              (not= (Character/toUpperCase (first i)) (first j)) (rec (subs i 1) j rec)
              :else (or (rec (subs i 1) (subs j 1) rec)
                        (rec (subs i 1) j rec))))
        mh (memoize h)]
    (mh to-modify to-match mh)))

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
