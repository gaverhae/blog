(ns t.day16
  (:require [clojure.core.match :refer [match]]))

(defn parse-bits
  [bits]
  (letfn [(parse-packet [bits]
            (let [version (Long/parseLong (subs bits 0 3) 2)
                  type (Long/parseLong (subs bits 3 6) 2)
                  bits (subs bits 6)]
              (if (= type 4)
                (let [[value bits] (parse-value bits)]
                  [[:literal version value]
                   bits])
                (let [[args bits] (parse-args bits)]
                  [[:operator version type args]
                   bits]))))
          (parse-value [bits]
            (loop [bits bits
                   so-far ""]
              (let [continue? (= "1" (subs bits 0 1))
                    so-far (str so-far (subs bits 1 5))
                    bits (subs bits 5)]
                (if continue?
                  (recur bits so-far)
                  [(Long/parseLong so-far 2) bits]))))
          (parse-args [bits]
            (let [length-type ({"0" :bits, "1" :packets} (subs bits 0 1))
                  bits (subs bits 1)]
              (case length-type
                :bits (let [length (Long/parseLong (subs bits 0 15) 2)
                            bits (subs bits 15)
                            packets (subs bits 0 length)
                            bits (subs bits length)]
                        [(loop [so-far []
                                bits packets]
                           (if (empty? bits)
                             so-far
                             (let [[p bits] (parse-packet bits)]
                               (recur (conj so-far p) bits))))
                         bits])
                :packets (let [length (Long/parseLong (subs bits 0 11) 2)
                               bits (subs bits 11)]
                           (loop [so-far []
                                  bits bits]
                             (if (= (count so-far) length)
                               [so-far bits]
                               (let [[p bits] (parse-packet bits)]
                                 (recur (conj so-far p) bits))))))))]
    (let [[top-level-packet bits] (parse-packet bits)]
      (when-not (every? #{\0} bits)
        (throw (RuntimeException. ^String bits)))
      top-level-packet)))

(defn parse
  [lines]
  (->> lines
       first
       (mapcat {\0 "0000" \1 "0001" \2 "0010" \3 "0011" \4 "0100" \5 "0101"
                \6 "0110" \7 "0111" \8 "1000" \9 "1001" \A "1010" \B "1011"
                \C "1100" \D "1101" \E "1110" \F "1111"})
       (apply str)
       parse-bits))

(defn part1
  [input]
  (match input
    [:literal v _] v
    [:operator v _ args] (reduce + v (map part1 args))))

(defn part2
  [input]
  (let [f (fn [f] (comp {true 1, false 0} f))]
    (match input
      [:literal _ n] n
      [:operator _ op args] (apply (get [+ * min max nil (f >) (f <) (f =)] op) (map part2 args)))))
