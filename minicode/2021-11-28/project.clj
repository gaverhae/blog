(defproject t "app"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [criterium "0.4.6"]]
  :main ^:skip-aot t.core
  :target-path "target/%s"
  :global-vars {*warn-on-reflection* true}
  :profiles {:uberjar {:aot :all}})
