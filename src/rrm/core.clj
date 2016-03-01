(ns rrm.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.json :as ring-json]
            [ring.util.response	:as rr]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.cors :refer [wrap-cors]]
            [bouncer.core :as b]
            [bouncer.validators :as v]
            [clj-time.core :as t]
            [compojure.response :refer [render]]
            [clojure.java.io :as io]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clojure.string :as st]
            [crypto.password.bcrypt :as pwds]
            )
  (:gen-class))

(defn home
  ""
  [req]
  (render (io/resource "index.html") req))


(defn normal-user
  ""
  [req]
  (render (io/resource "index1.html") req))

(defroutes app-routes
  (GET "/" [] home)
  (GET "/citizens" [] normal-user)
  (route/resources "/static")
  (route/not-found "<h1>Page not found</h1>"))

(def app
  (-> app-routes
      (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :put :post :delete])
      (ring-json/wrap-json-body)
      (ring-json/wrap-json-response)))

(defn -main
  "Record Room Management System "
  [& args]
  (jetty/run-jetty app {:port 8080
                        :join? false}))
