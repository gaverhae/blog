(defproject t "app"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [criterium "0.4.6"]]
  :main ^:skip-aot t.core
  :global-vars {*warn-on-reflection* true
                *unchecked-math* :warn-on-boxed}
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
