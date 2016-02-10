(ns rrm.login
  (:require-macros [secretary.core :refer [defroute]])
  (:require [goog.events :as events]
            [goog.dom :as dom]
            [goog.history.EventType :as EventType]
            [bouncer.core :as b]
            [bouncer.validators :as v]
            [secretary.core :as secretary]
            [reagent.core :as reagent :refer [atom render]])
  (:import goog.History))


(defn validator [data-set]
  (first (b/validate data-set
                     :username-email [[v/required :message "Filed is required"]
                                      [v/email :message "Enter valid email-id"]]
                     :password [[v/required :message "Filed is required"]
                                [v/string  :message "Enter valid password"]])))

(defn input-element [id ttype data-set placeholder in-focus]
  [:input#id.form-control {:type ttype
                           :value (@data-set id)
                           :placeholder placeholder
                           :on-change #(swap! data-set assoc id (-> % .-target .-value))
                           :on-blur  #(reset! in-focus true)
                           }])

(defn input-validate [id label span-class ttype data-set placeholder focus]
  (let [input-focus (reagent/atom nil)]
    (fn []
      [:div.form-group
       [:div.col-md-12
        [:label label]
        [:div.input-group.col-sm-10
         [:span {:class span-class}]
         [input-element id ttype data-set placeholder input-focus]]
        (if (or @input-focus @focus)
          (if (= nil (validator @data-set))
            [:div]
            [:div {:style  {:color "red"}}
             [:b
              (str (first ((validator @data-set) id)))]]  )
          [:div])]])))

(defn button [value ttype data-set focus]
  [:div.form-group
   [:div.col-md-6
    [:button.btn.btn-primary {:type ttype
                              :on-click (fn []
                                          (if (= nil (validator @data-set))
                                            (js/alert "You are successfully Registered")
                                            (reset! focus true)))} value]]])

(defn home []
  (let [my-data (reagent/atom  {})
        focus (reagent/atom nil)]
    (fn []
      [:div.container
       [:div.panel.panel-primary.modal-dialog
        [:div.panel-heading
         [:h2 "Log-in form"]]
        [:div.panel-body
         [input-validate :username-email "Username or Email"  "input-group-addon glyphicon glyphicon-user" "email" my-data "eg.,Siva or siva@***.com" focus]
         [input-validate :password "Password"  "input-group-addon glyphicon glyphicon-lock" "password" my-data "eg., .........." focus]
         [button "Sign-in" "button" my-data focus ]]]])))

(defn render-sample []
  (reagent/render-component [home]
                            (.getElementById js/document "app")))

(render-sample)
