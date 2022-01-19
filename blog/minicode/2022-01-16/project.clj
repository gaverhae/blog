(defproject t "app"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.taoensso/tufte "2.2.0"]]
  :main ^:skip-aot t.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
