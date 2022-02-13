(defproject t "app"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [criterium "0.4.6"]]
  :main ^:skip-aot t.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
