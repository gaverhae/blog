(ns blog.logs
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as insta]
            [clojure.core.match :refer [match]]))

(def parse-access-line
  (insta/parser
    "<S> = ip <' - - ['> date <'] \"'> (method <' '> path <' '> http)? <'\" '> code <' '> bytes-sent <' \"'> referrer <'\" \"'> user-agent <'\"'>
     ip = #'\\d+(\\.\\d+){3}'
     date = #'\\d+' <'/'> #'[A-Z][a-z]{2}' <'/'> #'\\d{4}' <':'> #'\\d{2}' <':'> #'\\d{2}' <':'> #'\\d{2}' <' +0000'>
     method = #'[A-Z]*'
     path = #'/[^ ]*'
     http = #'[^\"]*'
     code = #'\\d+'
     bytes-sent = #'\\d+'
     referrer = #'[^\"]+'
     user-agent = #'[^\"]*'
    "))

(defn access-logs
  []
  (->> (io/file "logs")
       .listFiles
       (filter #(string/starts-with? (.getName %) "access.log"))
       (sort-by #(.getName %))
       (mapcat (fn [file]
                 (let [fin (java.io.FileInputStream. file)
                       gzin (java.util.zip.GZIPInputStream. fin)
                       rdr (io/reader gzin)]
                   (line-seq rdr))))
       (map parse-access-line)
       (map (fn [line]
              (let [m {"Jan" "01", "Feb" "02", "Mar" "03", "Apr" "04", "May" "05", "Jun" "06",
                       "Jul" "07", "Aug" "08", "Sep" "09", "Oct" "10", "Nov" "11", "Dec" "12"}]
                (reduce (fn [entry prop]
                          (assoc entry (first prop)
                                       (match prop
                                         [:date day month year hour min sec] (str year "-" (m month) "-" day "T" hour ":" min ":" sec "Z")
                                         [_ other] other)))
                        {}
                        line))))))

(defn per-day
  []
  (->> (access-logs)
       (take 30)
       (group-by #(-> % :date (subs 0 10)))
       (map (fn [[d logs]] [d (count logs)]))))

(comment

  (->> (access-logs)
       (remove :date))


         )
