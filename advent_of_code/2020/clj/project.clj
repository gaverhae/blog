(defproject t "app"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.match "1.0.0"]
                 [instaparse "1.4.10"]
                 [criterium "0.4.6"]]
  :global-vars {*warn-on-reflection* true}
  :jvm-opts ["-Xverify:none"]
  :repl-options {:init-ns t.core}
  :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]
  :test-refresh {:quiet true
                 :changes-only true})
