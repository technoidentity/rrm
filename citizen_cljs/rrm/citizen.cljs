(ns rrm.citizen
  (:require [goog.events :as events]
            [secretary.core :as secretary]
            [goog.net.XhrIo :as xhr]
            [reagent.core :as r]
            [cognitect.transit :as t]
            [goog.structs :as structs]
            [cljs-time.format :as f]
            [cljs-time.core :as tt]
            [cljs-time.coerce :as c]
            [cljs-time.predicates :as p]
            [cljsjs.react-bootstrap]
            [clojure.string :as st]
            [goog.dom :as dom]
            [goog.history.EventType :as EventType]
            [bouncer.core :as b]
            [bouncer.validators :as v])
  (:import goog.History
           goog.json.Serializer
           goog.date.Date
           goog.array))

(def serverhost "http://localhost:9000/")

(defonce citizen-storage (r/atom {:mutations {}
                                  :current-page 1
                                  :total-pages 0
                                  :page-location nil
                                  :districts nil
                                  :subdivisions nil
                                  :villages {}
                                  :o2mutations {}
                                  :o4mutations {}
                                  :o6mutations {}
                                  :token ""
                                  :is-searched-results false
                                  :user nil
                                  :message nil}))



(defn set-key-value [k v]
  (reset! citizen-storage (assoc @citizen-storage k v)))

(defn get-value! [k]
  (k @citizen-storage))

(defn getdata [res]
  (.getResponseJson (.-target res)))

(defn http-get [url callback]
  (xhr/send url callback))

(defn get-total-rec-no [nos]
  (let [totrec (quot nos 10)]
    (if (zero? (mod nos 10))
      totrec
      (+ 1 totrec))))

(declare render-mutations)

(def button-tool-bar (r/adapt-react-class (aget js/ReactBootstrap "ButtonToolbar")))
(def button (r/adapt-react-class (aget js/ReactBootstrap "Button")))
(def pager-elem (r/adapt-react-class (aget js/ReactBootstrap "Pagination")))

(defn get-search-url [vid mutno fp
                      sp nop kkn
                      o2n tit kn
                      o4n o6n]
  (let [purl (str serverhost "mutations/search?")]

    (cond (not (st/blank? mutno)) (str purl "mutationNo="mutno)
          :else (str purl "villageId="(int vid)"&firstparty="fp"&secondparty="sp"&nameofpo="nop"&khatakhatuninumber="kkn"&o2number="o2n"&title="tit"&khasranumber="kn"&o4number="o4n"&o6number="o6n))))


(defn get-index-url [is-searched-results sel-page vid
                     mutno fp sp nop kkn o2n tit kn
                     o4n o6n]
  (cond (= true is-searched-results)(str (get-search-url vid mutno fp sp nop kkn o2n tit kn o4n o6n)"&pageIndex="sel-page"&pageSize=10")
        :else (str serverhost "mutations?pageIndex="sel-page "&pageSize=10")))


(defn set-pager-data [sel-page-no]
  (let [mn   (.-value (.getElementById js/document "mutationnumber"))
        st   (.-value (.getElementById js/document "stitle"))
        vid  (.-value (.getElementById js/document "src-vill"))
        po   (.-value (.getElementById js/document "svillagename"))
        so2  (.-value (.getElementById js/document "so2number"))
        so4  (.-value (.getElementById js/document "so4number"))
        so6  (.-value (.getElementById js/document "so6number"))
        knum (.-value (.getElementById js/document "skhasranumber"))
        kknum (.-value (.getElementById js/document "skhatakhatuninumber"))
        fp (.-value (.getElementById js/document "snameofthefirstparty"))
        sp (.-value (.getElementById js/document "snameofthesecondparty"))
        onres (fn[json]
                (let [dt (getdata json)]
                  (set-key-value :mutations (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (r/render [render-mutations (get-value! :mutations)]
                            (.getElementById js/document "app1"))))]
    (http-get (get-index-url (get-value! :is-searched-results)
                             (dec (get-value! :current-page))
                             vid mn fp sp po
                             kknum so2 st knum
                             so4 so6)
              onres)))

(defn pager [value total-rec]
  [pager-elem {:bsSize "large"
               :prev true
               :next true
               :first true
               :last true
               :ellipsis true
               :items (:total-pages @citizen-storage)
               :activePage (:current-page @citizen-storage)
               :maxButtons 5
               :onSelect (fn [s1 s2]
                           (do
                             (set-key-value :current-page (.-eventKey s2))
                             (set-pager-data (get-value! :current-page))))}])


(defn shared-state [totalRec]
  (let [val (r/atom 1)
        trec (r/atom totalRec)]
    [:div.row
     [pager val trec]]))

(defn datalist []
  [:datalist {:id "combo"}
   (let [name-po ["Kumar" "Sai" "Bhaskar" "Rajesh"]]
     (for [i name-po]
       ^{:key i}
       [:option {:value i}]))])

(defn src-dist-onchange [val]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (set-key-value :subdivisions dt)
                (set-key-value :villages nil)))]
    (http-get (str  serverhost "districts/" val  "/subdivisions") res)))

(defn src-dist-sel-tag []
  [:select.form-control {:id :src-dist
                         :placeholder "District Name"
                         :on-change #(src-dist-onchange (-> % .-target .-value)) }
   [:option {:value 0} "--Select--"]
   (for [d  (@citizen-storage :districts)]
     ^{:key (.-id d)}
     [:option {:value (.-id d)} (.-name d)])])

(defn src-sub-onchange [ val]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (set-key-value :villages dt)))]
    (http-get (str serverhost "subdivisions/" val  "/villages") res)))

(defn src-sub-sel-tag []
  [:select.form-control {:id :src-sub
                         :placeholder "Sub Division Name"
                         :on-change  #(src-sub-onchange (-> % .-target .-value)) }
   [:option {:value 0} "--Select--"]
   (for [d  (@citizen-storage :subdivisions)]
     ^{:key (.-id d)}
     [:option {:value (.-id d)} (.-subdivisionname d)])])

(defn src-vill-sel-tag []
  [:select.form-control {:id :src-vill
                         :placeholder "Village Name"
                         }
   [:option {:value 0} "--Select--"]
   (for [d  (@citizen-storage :villages)]
     ^{:key (.-id d)}
     [:option {:value (.-id d)} (.-villagename d)])])



(defn search [event]
  (let [mn   (.-value (.getElementById js/document "mutationnumber"))
        st   (.-value (.getElementById js/document "stitle"))
        vid  (.-value (.getElementById js/document "src-vill"))
        po   (.-value (.getElementById js/document "svillagename"))
        so2  (.-value (.getElementById js/document "so2number"))
        so4  (.-value (.getElementById js/document "so4number"))
        so6  (.-value (.getElementById js/document "so6number"))
        knum (.-value (.getElementById js/document "skhasranumber"))
        kknum (.-value (.getElementById js/document "skhatakhatuninumber"))
        fp (.-value (.getElementById js/document "snameofthefirstparty"))
        sp (.-value (.getElementById js/document "snameofthesecondparty"))
        onres (fn [json] (let [data (getdata json)]
                           (if (empty? (.-data data))
                             (do 
                               (set-key-value :message "No Records to Display")
                               (set-key-value :mutations nil)
                               (set-key-value :total-pages (get-total-rec-no (.-pagesCount data)))
                               (r/render [render-mutations (get-value! :mutations)]  (.getElementById js/document "app1")))
                             (do
                               (set-key-value :message nil)
                               (set-key-value :mutations (.-data data))
                               (set-key-value :total-pages (get-total-rec-no (.-pagesCount data)))
                               (r/render [render-mutations (get-value! :mutations)]  (.getElementById js/document "app1"))))))]
    (set-key-value :current-page 1)
    (set-key-value :is-searched-results true)
    (http-get (str (get-search-url
                    vid mn fp sp po
                    kknum so2 st knum so4 so6)"&pageIndex=0&pageSize=10") onres)))

(defn tags-template [data]
  [:datalist {:id "mutationlist"}
   (for [d data]
    ^{:key d}
     [:option {:value d}])])


(defn on-change [val]
  (let [onresp (fn [json]
                 (let [dt (getdata json)]
                   (r/render [tags-template dt] (.getElementById js/document "tagdiv"))))]
    (http-get (str serverhost  "mutations/pluck?column=mutationnumber&value=" val) onresp)))


(defn render-mutations [mutations]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:div.page-header [:h3 "List of Mutations"]]]
    [:div.col-md-12
     [:div.form-group
      [:div.row
       [:div.col-sm-2 "Mutation Number"
        [:input.form-control  {:id "mutationnumber"
                               :list "mutationlist"
                               :type "text"
                               :placeholder "Enter Search by Mutation Number"
                               :on-change #(on-change (-> % .-target .-value))}
         [:div#tagdiv tags-template]]]]]
     [:div.form-group
      [:div.row
       [:div.col-sm-2 "District Name"
        [src-dist-sel-tag]]
       [:div.col-sm-2 "Sub Division Name"
        [src-sub-sel-tag]]
       [:div.col-sm-2 "Village Name"
        [src-vill-sel-tag]]
       [:div.col-sm-2 "O2 Number"
        [:input.form-control {:id "so2number"
                              :type "text"
                              :placeholder "O2 Number"}]]
       [:div.col-sm-2 "O4 Number"
        [:input.form-control {:id "so4number"
                              :type "text"
                              :placeholder "O4 Number"}]]
       [:div.col-sm-2 "O6 Number"
        [:input.form-control {:id "so6number"
                              :type "text"
                              :placeholder "O6 Number"}]]
       ]]

     [:div.form-group
      [:div.row
       [:div.col-sm-2 "Name of the First Party"
        [:input.form-control {:id "snameofthefirstparty"
                              :type "text"
                              :placeholder "Name of the First Party"}]]
       [:div.col-sm-2 "Name of the Second Party"
        [:input.form-control {:id "snameofthesecondparty"
                              :type "text"
                              :placeholder "Name of the Second Party"}]]
       [:div.col-sm-2 "Name of P.O"
        [:input.form-control {:id "svillagename"
                              :list "combo"
                              :placeholder "Name of P.O"}[datalist]]]
       [:div.col-sm-2 "Title"
        [:input.form-control {:id "stitle"
                              :type "text"
                              :placeholder "Title"}]]
       [:div.col-sm-2 "Khasra Number"
        [:input.form-control {:id "skhasranumber"
                              :type "text"
                              :placeholder "Khasra Number"} ]]
       [:div.col-sm-2 "Khata khatuni Number"
        [:input.form-control {:id "skhatakhatuninumber"
                              :type "text"
                              :placeholder "Khata Khatuni Number"}]]]]
     [:hr]
     [:div.form-group
      [:div.row
       [:div.col-sm-8.col-md-offset-5
        [button-tool-bar
         [button {:bs-style "primary" :on-click search } "Search"]
         ]]]]]

    [:div {:class "box-body"}
     [:table {:class "table table-bordered table-striped dataTable"}
      [:thead
       [:tr
        [:th "Mutation Number"]
        [:th "Name of the First Party"]
        [:th "Name of the Second Party"]
        [:th "Date of Institution"]
        [:th "Name of P.O"]
        [:th "Name of District"]
        [:th "Sub Division Name"]
        [:th "Name of Village"]
        [:th "O2 Number"]
        [:th "O4 Number"]
        [:th "O6 Number"]
        [:th "Sent Date"]
        [:th "Received Date "]
        ]]
      [:tbody
       (doall (for [mt mutations]
                ^{:key (.-id mt)} [:tr
                                   [:td (.-mutationnumber (.-numbers mt))]
                                   [:td (.-nameofthefirstparty mt)]
                                   [:td (.-nameofthesecondparty mt)]
                                   [:td (.-dateofinstitution mt)]
                                   [:td (.-nameofpo mt)]
                                   [:td (.-name (.-district  mt))]
                                   [:td (.-name (.-subdivision mt))]
                                   [:td (.-name (.-village mt))]
                                   [:td (.-o2number (.-numbers  mt))]
                                   [:td (.-o4number (.-numbers mt))]
                                   [:td (.-o6number (.-numbers mt))]
                                   [:td (.-senddate mt)]
                                   [:td (.-receiveddate mt)]
                                   ]))]]
     [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]
     [:div [:b [:h2 {:style  {:color "red"}} (str (:message @citizen-storage))]]]]]])




(defn main []
  (let [onres (fn [json] ((set-key-value :districts (getdata json))
                         (r/render [render-mutations]
                                   (.getElementById js/document "app1"))))]
    (http-get (str serverhost "districts") onres)))

(main)
