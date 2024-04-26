(ns cryogen.server
  (:require
    [clojure.edn :as edn]
    [clojure.string :as string]
    [compojure.core :as cj]
    [compojure.route :as route]
    [ring.util.response :refer [redirect file-response]]
    [ring.util.codec :refer [url-decode]]
    [ring.server.standalone :as ring-server]
    [cryogen-core.watcher :refer [start-watcher! start-watcher-for-changes!]]
    [cryogen-core.plugins :refer [load-plugins]]
    [cryogen.compiler :refer [compile-assets-timed]]
    [cryogen-core.config :refer [resolve-config]]
    [cryogen-core.io :refer [path]]))

(defn init
  [{:as config}]
  (load-plugins)
  (compile-assets-timed config)
  (let [ignored-files (-> config :ignored-files)
        recompile (fn [] (compile-assets-timed config))]
    (run! #(start-watcher! % ignored-files recompile)
          ["content" "themes"])))

(defn wrap-subdirectories
  [handler config]
  (fn [request]
    (let [{:keys [clean-urls blog-prefix public-dest]} config
          req-uri (.substring (url-decode (:uri request)) 1)
          res-path (if (or (.endsWith req-uri "/")
                           (.endsWith req-uri ".html")
                           (-> (string/split req-uri #"/")
                               last
                               (string/includes? ".")
                               not))
                     (condp = clean-urls
                       :trailing-slash (path req-uri "index.html")
                       :no-trailing-slash (if (or (= req-uri "")
                                                  (= req-uri "/")
                                                  (= req-uri
                                                     (if (string/blank? blog-prefix)
                                                       blog-prefix
                                                       (.substring blog-prefix 1))))
                                            (path req-uri "index.html")
                                            (path (str req-uri ".html")))
                       :dirty (path (str req-uri ".html")))
                     req-uri)]
      (or (file-response res-path {:root public-dest})
          (handler request)))))

(defn routes
  [config]
  (cj/routes
    (cj/GET "/" [] (redirect (path (:blog-prefix config)
                                   (when (= (:clean-urls config) :dirty)
                                     "index.html"))))
    (route/files "/")
    (route/not-found "Page not found")))

(defn serve
  [config]
  (init config)
  (ring-server/serve (wrap-subdirectories (routes config) config)))

(defn -main [& args]
  (let [cli-args (edn/read-string (first args))
        file-config (resolve-config)]
    (serve (merge file-config
                  {:port 3000}
                  cli-args))))
