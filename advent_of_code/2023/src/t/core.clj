(ns t.core
  (:gen-class)
  (:require [t.day12]))

(defn -main
  [& args]
  (println "Warming.")
  (t.day12/benchmark)
  (println "Ready")
  (read-line)
  (t.day12/benchmark))
