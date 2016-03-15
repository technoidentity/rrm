(defproject rrm "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [compojure "1.3.4"]
                 [migratus "0.8.6"]
                 [org.postgresql/postgresql "9.4-1203-jdbc42"]
                 [org.clojure/java.jdbc "0.4.2"]
                 [ring/ring-defaults "0.1.2"]
                 [ring/ring-jetty-adapter "1.4.0"]
                 [korma "0.4.0"]
                 [ring/ring-json "0.4.0"]
                 [org.clojure/clojurescript "0.0-2985"]
                 [secretary "1.2.3"]
                 [reagent "0.5.1"]
                 [yesql "0.5.1"]
                 [com.cognitect/transit-cljs "0.8.225"]
                 [ring-cors "0.1.7"]
                 [bouncer "0.3.3"]
                 [clj-time "0.11.0"]
                 [crypto-password "0.1.3"]
                 [cljsjs/react-bootstrap "0.28.1-0"
                  :exclusions [org.webjars.bower/jquery]]
                 [com.andrewmcveigh/cljs-time "0.4.0"]
                 [hodgepodge "0.1.3"]
                 [venantius/accountant "0.1.7"]
                 [org.eclipse.jetty/jetty-server "9.3.3.v20150827"]]

  :min-lein-version "2.5.2"
  :plugins [[lein-ring "0.8.13"]
            [migratus-lein "0.1.7"]
            [lein-cljsbuild "1.0.4"]]
  :source-paths ["src"]
  :main rrm.core
  :ring {:handler rrm.handler/app}
  :migratus {:store :database
             :migration-dir "migrations/"
             :db {:classname "org.postgresql.Driver"
                  :subprotocol "postgresql"
                  :subname "//localhost:5432/rrm"
                  :user "postgres"
                  :password "Design_20"}}

  :uberjar-name "rrm-0.1.0-SNAPSHOT-standalone.jar"
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}
   :uberjar {:aot :all}
   }

  :cljsbuild {:builds
              [{:id "app"
                :source-paths ["src_cljs/"]
                :compiler {:output-to "resources/public/js/app.js"
                           :output-dir "resources/public/js/out"
                           :source-map true
                           :optimizations :none
                           :asset-path "/static/js/out"
                           :main "rrm.corecljs"
                           :pretty-print true}}
               {:id "app1"
                :source-paths ["citizen_cljs/"]
                :compiler {:output-to "resources/public/js/app1.js"
                           :output-dir "resources/public/js/out1"
                           :source-map true
                           :optimizations :none
                           :asset-path "/static/js/out1"
                           :main "rrm.citizen"
                           :pretty-print true}}]})
