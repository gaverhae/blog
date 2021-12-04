(ns t.util)

(defn parse-integers
  [lines]
  (vec (map #(Long/parseLong %) lines)))

(defn transpose
  [s]
  (apply mapv vector s))
