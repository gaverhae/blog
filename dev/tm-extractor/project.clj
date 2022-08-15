(defproject tm-extractor "app"
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :global-vars {*warn-on-reflection* true}
  :main ^:skip-aot tm-extractor.core
  :profiles {:uberjar {:aot :all}}
  :repl-options {:init-ns tm-extractor.core})
