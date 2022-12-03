(ns recov.core
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

#_(defn recov
  [device target]
  (with-open [xin (io/input-stream (io/file device))
              xout (io/output-stream (io/file target))]
    (let [buf (make-array Byte/TYPE 1024)
          log (str target ".log")]
      (.skip xin (* 1024 577000))
      (loop [i 0]
        (let [size (try (.read xin buf)
                     (catch Exception _ -2))]
            (cond (pos? size)
                  (do (.write xout buf 0 size)
                      (recur (inc i)))
                  (== -2 size)
                  (do
                    (spit log (format "Error: %d\n" i) :append true)
                    (recur (inc i)))
                  :else nil))))))

#_(defn find-bad
  [device]
  (loop [pos (* 1024 500)]
    (let [res (try
                (let [buf (make-array Byte/TYPE 1024)]
                  (with-open [xin (io/input-stream (io/file device))]
                  (.skip xin (* pos 1024))
                  (.read xin buf)))
                true
                (catch Exception _
                  (println (format "Could not read byte %d" pos))
                  false))]
      (when (zero? (rem pos 1024))
        (println (format "Got to %5d MB (pos = %d)" (quot pos 1024) pos)))
      (when res
        (recur (inc pos))))))

#_(defn find-bad
  [device]
  (loop [pos (- 591098880 2000)]
    (let [res (try
                (with-open [xin (io/input-stream (io/file device))]
                  (.skip xin pos)
                  (.read xin))
                (println (format "read byte %d" pos))
                true
                (catch Exception _
                  (println (format "Could not read byte %d" pos))
                  false))]
      (when res
        (recur (inc pos))))))

#_(defn read-kb
  [dev pos ^bytes buf]
  (try
    (with-open [xin (io/input-stream (io/file dev))]
      #_(throw (RuntimeException.))
      (.skip xin pos)
      (let [size (.read xin buf)]
        [size []]))
    (catch Exception _
      (loop [offset 0
             failed []
             done? false]
        (if (or done? (== (alength buf) offset))
          [offset failed]
          (let [b (try (with-open [xin (io/input-stream (io/file dev))]
                         (.skip xin (+ pos offset))
                         (.read xin))
                       (catch Exception _ -2))]
            (case (int b)
              -1 (recur offset failed true)
              -2 (do (aset buf offset (byte 0))
                     (recur (inc offset) (conj failed offset) done?))
              (do (aset buf offset (unchecked-byte b))
                  (recur (inc offset) failed done?)))))))))

(comment


    (def one-at-a-time
      (let [buf (make-array Byte/TYPE (* 64 1024))]
      (read-block "/dev/disk4s3" 0 (fn [a & b] (println a b)) buf)
      (seq buf)))

    (def all-at-once
      (let [buf (make-array Byte/TYPE (* 64 1024))]
      (read-block "/dev/disk4s3" 0 (fn [a & b] (println a b)) buf)
      (seq buf)))

    (take 10 one-at-a-time)
    (take 10 all-at-once)
    (take 10 (map (comp unchecked-byte #(+ 128 %)) one-at-a-time))
    (reduce + (map - one-at-a-time all-at-once))

  )

#_(defn recov
  [device target]
  (with-open [xout (io/output-stream (io/file target))]
    (let [log (str target ".log")
          buf (make-array Byte/TYPE (* 64 1024))]
      (loop [pos 0]
        (let [[^long size failed] (read-kb device pos buf)]
          (when (seq failed)
            (spit log (format "Failed to read byte(s) %s\n" (pr failed))
                  :append true)
            (.write xout buf 0 (first failed))
            (System/exit 1))
          (if (== -1 size)
            (println "Done.")
            (do
              (.write xout buf 0 size)
              (recur (+ pos size)))))))))

#_(defn read-block
  [dev pos log ^bytes out]
  (try
    (with-open [xin (io/input-stream (io/file dev))]
      (.skip xin pos)
      (.read xin out))
    (catch Exception _
      (log "Failed to read block at %d" pos)
      (let [line ^bytes (make-array Byte/TYPE 1024)]
        (loop [offset 0]
          (if (== offset (* 64 1024))
            offset
            (let [^long b (try (with-open [xin (io/input-stream (io/file dev))]
                                 (.skip xin (+ pos offset))
                                 (let [size (.read xin line)]
                                   (log "  Successfully read line at %d" (+ pos offset))
                                   size))
                               (catch Exception _
                                 (log "  Failed to read line at %d" (+ pos offset))
                                 (loop [line-offset 0]
                                   (if (== 1024 line-offset)
                                     line-offset
                                     (let [one-byte (try (with-open [xin (io/input-stream (io/file dev))]
                                                           (.skip xin (+ pos offset line-offset))
                                                           (let [v (.read xin)]
                                                             (log "    Read byte at %d" (+ pos offset line-offset))
                                                             v))
                                                         (catch Exception _
                                                           (log "    Failed to read byte at %d, replacing with zero." (+ pos offset line-offset))
                                                           0))]
                                       (if (== -1 one-byte)
                                         line-offset
                                         (do (aset line line-offset (unchecked-byte one-byte))
                                             (recur (inc line-offset)))))))))]
              (if (pos? b)
                (do (System/arraycopy line 0 out offset b)
                    (recur (+ b offset)))
                offset))))))))

#_(defn recov
  [device target]
  (let [log (let [t (str target ".log")]
              (spit t (str "start at "
                           (.. (java.time.ZonedDateTime/now)
                               (format java.time.format.DateTimeFormatter/ISO_INSTANT))
                           "\n"))
              (fn [tpl i]
                (spit t (format (str tpl "\n") i) :append true)))
        buf (make-array Byte/TYPE (* 64 1024))]
    (with-open [xout (io/output-stream (io/file target))]
      (loop [pos 0]
        (let [^long size (read-block device pos log buf)]
          (when (pos? size)
            (.write xout buf 0 size)
            (recur (+ pos size))))))))

#_(defn read-block
  [dev pos log ^bytes out]
  (try
    (with-open [xin (io/input-stream (io/file dev))]
      (.skip xin pos)
      (.read xin out))
    (catch Exception _
      (log "Failed to read block at %20d" pos)
      (let [line ^bytes (make-array Byte/TYPE 1024)]
        (loop [offset 0]
          (if (== offset (* 64 1024))
            offset
            (let [^long b (try (with-open [xin (io/input-stream (io/file dev))]
                                 (.skip xin (+ pos offset))
                                 (let [size (.read xin line)]
                                   (log "  Successfully read line at %20d + %5d" pos offset)
                                   size))
                               (catch Exception _
                                 (let [word ^bytes (make-array Byte/TYPE 64)
                                       zeroes ^bytes (make-array Byte/TYPE 64)]
                                   (log "  Failed to read line at    %20d + %5d" pos offset)
                                   (loop [line-offset 0]
                                     (if (== 1024 line-offset)
                                       line-offset
                                       (let [size (try (with-open [xin (io/input-stream (io/file dev))]
                                                           (.skip xin (+ pos offset line-offset))
                                                           (let [size (.read xin word)]
                                                             (log "    Read word at %20d + %5d + %5d" pos offset line-offset)
                                                             size))
                                                         (catch Exception _
                                                           (log "    Failed to read word at %20d + %5d + %5d, replacing with zeroes." pos offset line-offset)
                                                           -1))]
                                       (cond (zero? size)
                                             line-offset
                                             (== -1 size)
                                             (do (System/arraycopy word 0 line line-offset 64)
                                                 (recur (+ 64 line-offset)))
                                             :else
                                             (do (System/arraycopy word 0 line line-offset size)
                                                 (recur (+ 64 line-offset))))))))))]
              (if (pos? b)
                (do (System/arraycopy line 0 out offset b)
                    (recur (+ b offset)))
                offset))))))))

#_(defn recov
  [device target]
  (let [log (let [t (str target ".log")]
              (spit t (str "start at "
                           (.. (java.time.ZonedDateTime/now)
                               (format java.time.format.DateTimeFormatter/ISO_INSTANT))
                           "\n"))
              (fn [tpl & is]
                (spit t (apply format (str tpl "\n") is) :append true)))
        buf (make-array Byte/TYPE (* 64 1024))]
    (with-open [xout (io/output-stream (io/file target))]
      (loop [pos 0]
        (let [^long size (read-block device pos log buf)]
          (when (pos? size)
            (.write xout buf 0 size)
            (recur (+ pos size))))))))




#_(defn read-block
  [dev pos log ^bytes out]
  (try
    (with-open [xin (io/input-stream (io/file dev))]
      (.skip xin pos)
      (.read xin out))
    (catch Exception _
      (log "Failed to read block at %20d" pos)
      (let [line ^bytes (make-array Byte/TYPE 1024)]
        (loop [offset 0]
          (if (== offset (* 64 1024))
            offset
            (let [^long b (try (with-open [xin (io/input-stream (io/file dev))]
                                 (.skip xin (+ pos offset))
                                 (let [size (.read xin line)]
                                   (log "  Successfully read line at %20d + %5d" pos offset)
                                   size))
                               (catch Exception _
                                 (log "  Failed to read line at    %20d + %5d, replacing with zeroes"
                                      pos offset)
                                 (java.util.Arrays/fill line (unchecked-byte 0))
                                 1024))]
              (if (pos? b)
                (do (System/arraycopy line 0 out offset b)
                    (recur (+ b offset)))
                offset))))))))

#_(defn recov
  [device target]
  (let [log (let [t (str target ".log")]
              (spit t (str "start at "
                           (.. (java.time.ZonedDateTime/now)
                               (format java.time.format.DateTimeFormatter/ISO_INSTANT))
                           "\n"))
              (fn [tpl & is]
                (spit t (apply format (str tpl "\n") is) :append true)))
        buf (make-array Byte/TYPE (* 64 1024))]
    (with-open [xout (io/output-stream (io/file target))]
      (loop [pos 0]
        (let [^long size (read-block device pos log buf)]
          (when (pos? size)
            (.write xout buf 0 size)
            (recur (+ pos size))))))))

;; done first run

(defn read-block
  [dev init-pos log ^bytes init-out q m]
  (let [f (fn f [pos ^bytes out]
            (try
              (with-open [xin (io/input-stream (io/file dev))]
                (.skip xin pos)
                (let [r (.read xin out)]
                  (log "S: %015X -> %015X - %010X" pos (+ pos (alength out)) (alength out))
                  r))
              (catch Exception _
                (log "F: %015X -> %015X - %010X" pos (+ pos (alength out)) (alength out))
                (let [s (alength out)]
                  (if (== s m)
                    (do
                      (log (str pos))
                      (java.util.Arrays/fill out (unchecked-byte 0))
                      s)
                    (let [line-length (quot s q)
                          line ^bytes (make-array Byte/TYPE line-length)]
                      (when-not (zero? (rem s q))
                        (println "ERROR" (pr-str [:s s :q q]))
                        (throw (Throwable. (pr-str [:s s :q q]))))
                      (loop [offset 0]
                        (if (== offset s)
                          offset
                          (let [r (f (+ pos offset) line)]
                            (if (pos? r)
                              (do (System/arraycopy line 0 out offset r)
                                  (recur (long (+ r offset))))
                              offset))))))))))]
    (f init-pos init-out)))

(defn recov
  [device target]
  (let [log (let [t (str device "-recov.log")]
              (spit t (str "start at "
                           (.. (java.time.ZonedDateTime/now)
                               (format java.time.format.DateTimeFormatter/ISO_INSTANT))
                           "\n"))
              (fn [tpl & is]
                (spit t (apply format (str tpl "\n") is) :append true)))
        buf (make-array Byte/TYPE (* 1024 1024 1024))]
    (with-open [xout (io/output-stream (io/file target))]
      (loop [pos 0]
        (let [^long size (read-block device pos log buf 16 1024)]
          (when (pos? size)
            (.write xout buf 0 size)
            (recur (+ pos size))))))))

(defn recov-p
  [device target log-file]
  (let [now-str (fn [] (.. (java.time.ZonedDateTime/now)
                           (format java.time.format.DateTimeFormatter/ISO_INSTANT)))
        log (fn [tpl & is]
              (spit log-file (apply format (str tpl "\n") is) :append true))]
    (spit log-file (format "start at %s\n" (now-str)))
    (let [chan (async/chan)
          reader (async/thread
                   (loop [pos 0]
                     (let [buf (make-array Byte/TYPE (* 256 1024 1024))
                           ^long size (read-block device pos log buf 2 1024)]
                       (when (pos? size)
                         (async/>!! chan [buf size])
                         (recur (+ pos size)))))
                   (async/close! chan))
          writer (async/thread
                   (with-open [xout (io/output-stream (io/file target))]
                     (loop []
                       (when-let [[buf size] (async/<!! chan)]
                         (.write xout buf 0 size)
                         (recur)))))]
      (async/<!! reader)
      (async/<!! writer)
      (log "end at %s" (now-str)))))

(defn diff
  [f1 f2]
  (let [buf1 ^bytes (make-array Byte/TYPE (* 1024 1024))
        buf2 ^bytes (make-array Byte/TYPE (alength buf1))
        print-array (fn [arr]
                      (->> arr
                           (map (fn [b] (format "%02X" b)))
                           (partition 32)
                           (map #(interpose " " %))
                           (map #(apply str %))
                           (interpose "\n")
                           (apply str)))]
    (with-open [in1 (io/input-stream (io/file f1))
                in2 (io/input-stream (io/file f2))]
      (loop [pos 0]
        (let [r1 (.read in1 buf1)
              r2 (.read in2 buf2)]
          (cond (and (== r1 r2)
                     (pos? r1))
                (do
                  (when (not (java.util.Arrays/equals buf1 buf2))
                    (print (format "%15d\n" pos))
                    (flush)
                    (spit "log1" (format "%15d\n%s\n\n" pos (print-array buf1))
                          :append true)
                    (spit "log2" (format "%15d\n%s\n\n" pos (print-array buf2))
                          :append true))
                  (recur (+ pos (alength buf1))))
                (== r1 r2 0)
                (println "Done.")
                :else
                (println (format "Error: different lengths: %d & %d, pod: %d" r1 r2 pos))))))))
