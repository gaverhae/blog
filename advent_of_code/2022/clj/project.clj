(defproject t "app"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.0"]
                 [instaparse "1.4.12"]
                 [criterium "0.4.6"]
                 [com.taoensso/tufte "2.4.5"]
                 [hato "0.9.0"]]
  :global-vars {*warn-on-reflection* true}
  :main ^:skip-aot t.core
  :jvm-opts ["-Xverify:none"]
  :plugins [[com.jakemccrary/lein-test-refresh "0.25.0"]]
  :target-path "target/%s"
  :test-refresh {:quiet true
                 :changes-only true}
  :test-paths ["src"]
  :repl-options {:init-ns t.core}
  :profiles {:uberjar {:aot :all}})
