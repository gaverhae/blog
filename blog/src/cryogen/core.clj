(ns cryogen.core
  (:require [cryogen.compiler :refer [compile-assets-timed]]
            [cryogen-core.config :refer [resolve-config]]
            [cryogen-core.plugins :refer [load-plugins]]))

(defn -main []
  (load-plugins)
  (compile-assets-timed (resolve-config))
  (System/exit 0))
