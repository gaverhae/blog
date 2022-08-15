(ns tm-extractor.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :refer [sh]])
  (:import [java.security MessageDigest DigestInputStream]))

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
    (fn [^java.io.File f] (.isDirectory f))
    (fn [^java.io.File f] (->> (.listFiles f)
                               (remove (fn [^java.io.File f]
                                         (java.nio.file.Files/isSymbolicLink (.toPath f))))))
    f))

(defn get-inode
  [^java.io.File f]
  (or (java.nio.file.Files/getAttribute (.toPath f) "unix:ino" (make-array java.nio.file.LinkOption 0))
      (throw (Exception. (str "Null inode for: " (.getAbsolutePath f))))))

(defn all-files
  [path]
  (let [norm (normalize path)]
    (->> (io/file path)
         (file-seq-no-link)
         (filter #(.isFile ^java.io.File %))
         (filter #(not= ".DS_Store" (.getName ^java.io.File %)))
         (map (fn [^java.io.File f]
                (let [p (.getAbsolutePath f)]
                  {:path (norm p)
                   :inode (get-inode f)}))))))

(defn extract
  [src dest]
  (let [src ^java.io.File (io/file src)
        dest ^java.io.File (io/file dest)
        files (->> (.listFiles src)
                   (remove (fn [^java.io.File f] (= "Latest" (.getName f))))
                   (map (fn [^java.io.File p] [(.getName p)
                                               (.getAbsolutePath p)]))
                   (mapcat (fn [[p ab]]
                             (->> (all-files ab)
                                  (map (fn [f] [p f]))))))]
    (loop [seen? #{}
           files files]
      (cond
        (empty? files) (println "Done.")
        :else
        (let [[bup {:as f :keys [path]}] (first files)]
          (when (not (seen? f))
            (sh "mkdir" "-p" (str dest "/" bup (.getParent (io/file path))))
            (sh "ln" (str src "/" bup path) (str dest "/" bup path)))
          (recur (conj seen? f)
                 (rest files)))))
    :done))

(defn -main
  [& _]
  (prn (extract "/Volumes/Manhattan/Backups.backupdb/gary-mbp"
                "/Volumes/Manhattan/extract")))
