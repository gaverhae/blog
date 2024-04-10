(defproject t "app"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :plugins [[com.jakemccrary/lein-test-refresh "0.23.0"]]
  :main ^:skip-aot t.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
