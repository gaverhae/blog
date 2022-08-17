(ns tm-extractor.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :refer [sh]])
  (:import [java.io File]
           [java.nio.file Files Paths LinkOption]))

(defn normalize
  [root]
  (let [abs-root (.getAbsolutePath (io/file root))
        rc (count abs-root)]
    (fn [abs-path]
      (if (not (string/starts-with? abs-path abs-root))
        (throw (Exception. "bleh"))
        (subs abs-path (count abs-root))))))

(defn file-seq-no-link
  [f]
  (tree-seq
    (fn [^File f] (.isDirectory f))
    (fn [^File f] (->> (.listFiles f)
                       (remove (fn [^File f]
                                 (Files/isSymbolicLink (.toPath f))))))
    f))

(defn get-inode
  [^File f]
  (or (Files/getAttribute (.toPath f) "unix:ino" (make-array LinkOption 0))
      (throw (Exception. (str "Null inode for: " (.getAbsolutePath f))))))

(defn all-files
  [path]
  (let [norm (normalize path)]
    (->> (io/file path)
         (file-seq-no-link)
         (filter #(.isFile ^File %))
         (filter #(not= ".DS_Store" (.getName ^File %)))
         (map (fn [^File f]
                (let [p (.getAbsolutePath f)]
                  {:path (norm p)
                   :inode (get-inode f)}))))))

(defn extract
  [src dest]
  (let [src ^File (io/file src)
        dest ^File (io/file dest)
        files (->> (.listFiles src)
                   (remove (fn [^File f] (= "Latest" (.getName f))))
                   (map (fn [^File p] [(.getName p)
                                       (.getAbsolutePath p)]))
                   (mapcat (fn [[p ab]]
                             (->> (all-files ab)
                                  (map (fn [f] [p f]))))))]
    (loop [seen? #{}
           i 0
           files files]
      (cond
        (empty? files) (println "Done.")
        :else
        (let [[bup {:as f :keys [path]}] (first files)
              new? (not (seen? f))]
          (when (zero? (rem i 100000))
            (println (format "%s: %12d %s %s %s"
                             (.format (java.time.ZonedDateTime/now)
                                      (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssx"))
                             i
                             (if new? "new" "old")
                             bup
                             (subs path 0 (min 100 (count path))))))
          (when new?
            (.mkdirs (io/file (str dest "/" bup (.getParent (io/file path)))))
            (try
              (Files/createLink (Paths/get (str dest "/" bup path) (make-array String 0))
                                (Paths/get (str src "/" bup path) (make-array String 0)))
              (catch Exception _
                (print (str "Error linking " path ".\n")))))
          (recur (conj seen? f)
                 (inc i)
                 (rest files)))))
    :done))

(defn -main
  [from to]
  (prn (extract from to)))
