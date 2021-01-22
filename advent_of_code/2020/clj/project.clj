(defproject t "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "CC BY-NC-ND 4.0"
            :url "https://creativecommons.org/licenses/by-nc-nd/4.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.match "1.0.0"]
                 [instaparse "1.4.10"]]
  :global-vars {*warn-on-reflection* true}
  :jvm-opts ["-Xverify:none"]
  :repl-options {:init-ns t.core}
  :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]
  :test-refresh {:quiet true
                 :changes-only true})
