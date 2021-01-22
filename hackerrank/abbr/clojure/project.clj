(defproject t "app"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "CC BY-NC-ND 4.0"
            :url "https://creativecommons.org/licenses/by-nc-nd/4.0/"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot t.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
