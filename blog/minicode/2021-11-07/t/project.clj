(defproject t "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [criterium "0.4.6"]]
  :main ^:skip-aot t.core
  :target-path "target/%s"
  :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]
  :test-refresh {:quiet true
                 :changes-only true}
  :global-vars {*warn-on-reflection* true}
  :profiles {:uberjar {:aot :all}})
