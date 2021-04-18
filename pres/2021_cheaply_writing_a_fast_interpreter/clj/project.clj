(defproject t "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [criterium "0.4.6"]
                 [datascript "1.0.7"]]
  :global-vars {*warn-on-reflection* true}
  :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]
  :test-refresh {:quiet true
                 :changes-only true}
  :repl-options {:init-ns t.core})
