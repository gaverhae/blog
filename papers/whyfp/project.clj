(defproject whyfp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "CC BY-NC-ND 4.0"
            :url "https://creativecommons.org/licenses/by-nc-nd/4.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.match "1.0.0"]]
  :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]
  :test-refresh {:quiet true
                 :changes-only true}
  :repl-options {:init-ns whyfp.core})
