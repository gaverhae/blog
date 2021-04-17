(defproject t "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [criterium "0.4.6"]
                 [datascript "1.0.7"]]
  :global-vars {*warn-on-reflection* true}
  :repl-options {:init-ns t.core})
