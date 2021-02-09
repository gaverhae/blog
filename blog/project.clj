(defproject my-blog "blog"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [ring/ring-devel "1.8.2"]
                 [compojure "1.6.2"]
                 [ring-server "0.5.0"]
                 [cryogen-flexmark "0.1.4"]
                 [cryogen-core "0.4.0"]
                 [instaparse "1.4.10"]
                 [org.clojure/core.match "1.0.0"]]
  :plugins [[lein-ring "0.12.5"]]
  :main cryogen.core
  :ring {:init cryogen.server/init
         :handler cryogen.server/handler}
  :aliases {"serve"      ["run" "-m" "cryogen.server"]
            "serve-fast" ["run" "-m" "cryogen.server" "fast"]})
