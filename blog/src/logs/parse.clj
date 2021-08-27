(ns logs.parse
  (:require [clojure.java.io :as io])
  (:import [java.util.zip GZIPInputStream]))

(defn read-dir
  "Given a path to a directory, reads all gzip lines in it and returns a seq of lines."
  [path]
  (->> path
       io/file
       .listFiles
       (mapcat (fn [f]
                 (->> f
                      io/input-stream
                      GZIPInputStream.
                      io/reader
                      line-seq)))))
