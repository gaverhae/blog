(ns t.util)

(defn parse-integers
  [lines]
  (vec (map #(Long/parseLong %) lines)))
