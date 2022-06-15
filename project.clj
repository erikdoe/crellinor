(defproject crellinor "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.incubator "0.1.4"]
                 [ring "1.8.0"]
                 [metosin/compojure-api "1.1.13"]
                 [com.taoensso/tufte "1.4.0"]
                 ]
  :ring {:init    crellinor.web/start
         :handler crellinor.web/app}
  :resource-paths ["resources"]
  :plugins [[lein-ring "0.12.5"]]
  :main ^:skip-aot crellinor.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
