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

(def combined
  "nginx log format, as a regex"
  #"([^ ]+) - (.*) \[(.*)\] \"([^\"]*)\" ([^ ]*) (\d+) \"([^\"]+)\" \"([^\"]+)\"")

(defn parse-line
  [line]
  (when-let [[_ from user ts req status size referer user-agent]
             (re-find combined line)]
    {:from from
     :user user
     :ts ts
     :req req
     :status status
     :size size
     :referer referer
     :user-agent user-agent}))

(defn get-logs
  []
  (let [raw (read-dir "logs")
        parsed (keep parse-line raw)]
    (- (count raw) (count parsed))))

(comment
  (get-logs)
1504
  )
