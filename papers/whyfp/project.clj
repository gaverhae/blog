(defproject whyfp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL0.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.match "1.0.0"]]
  :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]
  :test-refresh {:quiet true
                 :changes-only true}
  :repl-options {:init-ns whyfp.core})
