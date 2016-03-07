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
  (GET "/login" [] home)
  (GET "/mutations" [] home)
  (GET "/mutations/update/:id" [id] home)
  (GET "/mutations/add" [] home)
  (GET "/revenue" [] home)
  (GET "/revenue/add" [] home)
  (GET "/revenue/update/:id" [id] home)
  (GET "/khasragirdwani" [] home)
  (GET "/khasragirdwani/add" [] home)
  (GET "/khasragirdwani/update/:id" [id] home)
  (GET "/masavi" [] home)
  (GET "/masavi/add" [] home)
  (GET "/masavi/update/:id" [id] home)
  (GET "/consolidation" [] home)
  (GET "/consolidation/add" [] home)
  (GET "/consolidation/update/:id" [id] home)
  (GET "/fieldbook" [] home)
  (GET "/fieldbook/add" [] home)
  (GET "/fieldbook/update/:id" [id] home)
  (GET "/misc" [] home)
  (GET "/misc/add" [] home)
  (GET "/misc/update/:id" [] home)
  (GET "/o2register" [] home)
  (GET "/o2register/add" [] home)
  (GET "/o2register/update/:id" [id] home)
  (GET "/o4register" [] home)
  (GET "/o4register/add" [] home)
  (GET "/o4register/update/:id" [id] home)
  (GET "/o6register" [] home)
  (GET "/o6register/add" [] home)
  (GET "/o6register/update/:id" [id] home)
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
  (jetty/run-jetty app {:port 8193
                        :join? false}))
