(ns rrm.corecljs
  (:require-macros [secretary.core :refer [defroute]])
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

(defonce storage (r/atom {:mutations {}
                          :current-page 1
                          :total-pages 1
                          :page-location nil
                          :villages {}
                          :is-searched-results false
                          :user nil}))

(def serverhost "http://localhost:9000/")

(defn set-key-value [k v]
  (reset! storage (assoc @storage k v)))

(defn http-get [url callback]
  (xhr/send url callback))

(defn get-value! [k]
  (k @storage))

(defn page []
  (get-value! :page-location))

(defn getinputvalue[id]
  (.-value (.getElementById js/document id)))

(defn getdata [res]
  (.getResponseJson (.-target res)))

(defn get-status [res]
  (.getStatus (.-target res)))

(defn http-post [url callback data]
  (xhr/send url callback "POST" data  (structs/Map. (clj->js {:Content-Type "application/json"}))))

(defn http-put [url callback data]
  (xhr/send url callback "PUT" data  (structs/Map. (clj->js {:Content-Type "application/json"}))))

(defn http-delete [url callback]
  (xhr/send url callback "DELETE"  (structs/Map. (clj->js {:Content-Type "application/json"}))))


;; =====================================================================================================
;; login-form with validations
;; =====================================================================================================

(defn login-validator [data-set]
  (first (b/validate data-set
                     :email [[v/required :message "Filed is required"]
                             [v/email :message "Enter valid email-id"]]
                     :password [[v/required :message "Filed is required"]
                                [v/string  :message "Enter valid password"]])))

(defn input-element [id ttype data-set placeholder in-focus]
  ;;(js/console.log @data-set)
  [:input.form-control {:id id
                        :type ttype
                        :value (@data-set id)
                        :placeholder placeholder
                        :on-change #(swap! data-set assoc id (-> % .-target .-value))
                        :on-blur  #(reset! in-focus "on")
                        }])


(defn login-input-element [id label span-class ttype data-set placeholder focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:div.col-md-12
        [:label label]
        [:div.input-group.col-sm-10
         [:span {:class span-class}]
         [input-element id ttype data-set placeholder input-focus]]
        (if (or @input-focus @focus)
          (if (= nil (login-validator @data-set))
            [:div]
            [:div {:style  {:color "red"}} [:b (str (first ((login-validator @data-set) id)))]])
          [:div])]])))


(defn submit-login [data-set focus]
  (if (= nil (login-validator @data-set))
    (let [onresp (fn [json]
                   (if (= (get-status json) 200)
                     ((set-key-value :user (getdata json))
                      (secretary/dispatch! "/documents"))))]
      (http-post (str serverhost "user/authenticate")
                 onresp (.serialize (Serializer.) (clj->js @data-set ))))
    (reset! focus "on")))

(defn submit-button [data-set focus]
  [:div.form-group
   [:div.col-md-6
    [:button.btn.btn-primary {:on-click #(submit-login data-set focus)} "Submit" ]]])

(defn login []
  (let [my-data (r/atom  {})
        focus (r/atom nil)]
    (fn []
      [:div.container
       [:div.panel.panel-primary.modal-dialog
        [:div.panel-heading
         [:h2 "Log-in"]]
        [:div.panel-body
         [login-input-element :email "Email"  "input-group-addon glyphicon glyphicon-user" "email" my-data "Email" focus]
         [login-input-element :password "Password"  "input-group-addon glyphicon glyphicon-lock" "password" my-data "password" focus]
         [submit-button my-data focus ]]]])))

;; ====================================================================================================
;; end of login-form
;; ====================================================================================================

(defn is-authenticated? []
  (not (nil? (get-value! :user))))

(defn set-page! [currnt-page]
  (set-key-value :page-location
                 currnt-page))

(defn get-total-rec-no [nos]
  (let [totrec (quot nos 10)]
    (if (zero? (mod nos 10))
      totrec
      (+ 1 totrec))))

(defn get-range-data [data date1 date2]
  (filter #(tt/within? (tt/interval date1 (tt/plus date2 (tt/days 1)))
                       (c/from-string (.-date %))) data))

(defn included? [s subs]
  (>= (.indexOf s subs) 0))


(defn filter-by-str [data lstr]
  (filter #(or (included? (st/lower-case (.-title %)) lstr)
               (included? (st/lower-case (.-documentname %)) lstr))
          data))

(defn filter-by-str-dates
  ([data lstr date1] (filter #(and (or (included? (st/lower-case (.-title %)) lstr)
                                       (included? (st/lower-case (.-documentname %)) lstr))
                                   (tt/= (c/from-string (.-date %)) date1)) data))
  ([data lstr date1 date2](filter #(and (or (included? (st/lower-case (.-title %)) lstr)
                                            (included? (st/lower-case (.-documentname %)) lstr))
                                        (tt/within? (tt/interval date1 (tt/plus date2 (tt/days 1)))
                                                    (c/from-string (.-date %)))) data)))


(defn get-search-url [mn dt]
  (let [mnv (st/blank? mn)
        dtv (st/blank? dt)
        purl (str serverhost "mutations/search?")]
    (cond (and (not mnv) dtv ) (str purl "mutationNumber=" mn)
          (and mnv (not dtv))  (str purl  "value=" dt)
          :else (str purl "mutationNumber="mn"&value="dt))))

(defn get-index-url [is-searched-results sel-page mn dt]
  (cond (= true is-searched-results)(str (get-search-url mn dt)"&pageIndex="sel-page"&pageSize=10")
        :else (str serverhost "mutations?pageIndex="sel-page "&pageSize=10")))

(defn get-new-page-data [data current-page total-pages]
  (js/console.log total-pages)
  (let [pag-start (* 10 (dec current-page))
        pag-end (+ pag-start 9)]
    (cond (<= total-pages 1) (do (set-key-value :current-page 1)
                                 (set-key-value :total-pages 1)
                                 (clj->js (keep-indexed #(if (< %1 10) %2) data)))
          :else (do (set-key-value :total-pages total-pages)
                    (clj->js (keep-indexed
                              #(if (and (>= %1 pag-start) (<= %1 pag-end)) %2) data))))))


(defn url-format [url title]
  [:a {:href url :class "btn btn-primary  glyphicon glyphicon-plus"} title])

(def w (t/writer :json-verbose))

(defn filter-data [data date1 date2 search-str]
  (let [srcstrv (st/blank? search-str)
        lstr (st/lower-case search-str)]

    (cond (and (not (nil? date1)) (nil? date2) srcstrv) (filter #(tt/= date1 (c/from-string (.-date %))) data)
          (and (not (nil? date1)) (not (nil? date2)) srcstrv) (get-range-data data date1 date2)
          (and (nil? date1) (nil? date2) (not srcstrv)) (filter-by-str data lstr)
          (and (not (nil? date1)) (nil? date2) (not srcstrv)) (filter-by-str-dates data lstr date1)
          :else (filter-by-str-dates data lstr date1 date2))))

(def pager-elem (r/adapt-react-class (aget js/ReactBootstrap "Pagination")))

(defn set-pager-data [sel-page-no]
  (let [mt (.-value (.getElementById js/document "mutationnumber"))
        v (.-value (.getElementById js/document "dt"))
        onres (fn[json]
                (let [dt (getdata json)]
                  (set-key-value :mutations (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-key-value :page-location
                                 [render-mutations (get-value! :mutations)])))]
    (http-get (get-index-url (get-value! :is-searched-results)
                             (dec (get-value! :current-page))
                             mt v)
              onres)))

(defn pager [value total-rec]
  [pager-elem {:bsSize "large"
               :prev true
               :next true
               :first true
               :last true
               :ellipsis true
               :items (:total-pages @storage)
               :activePage (:current-page @storage)
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


(declare render-mutations)

(defn cancel [event]
  (secretary/dispatch! "/"))

;;...... Side Tab Events .........

(defn mut-click [event]
  (secretary/dispatch! "/"))

(defn rev-click [event]
  (secretary/dispatch! "/revenue"))

(defn khr-click [event]
  (secretary/dispatch! "/khasragirdwani"))

(defn masavi-click [event]
  (secretary/dispatch! "/masavi"))

(defn cons-click [event]
  (secretary/dispatch! "/consolidation"))

(defn field-click [event]
  (secretary/dispatch! "/fieldbook"))

(defn misc-click [event]
  (secretary/dispatch! "/misc"))

(defn o2-click [event]
  (secretary/dispatch! "/o2register"))

(defn o4-click [event]
  (secretary/dispatch! "/o4register"))

(defn o6-click [event]
  (secretary/dispatch! "/o6register"))

;;.............. End of Tab Events ......

(defn search [event]
  (let [mn  (.-value (.getElementById js/document "mutationnumber"))
        dt  (.-value (.getElementById js/document "dt"))
        onres (fn [json] (let [data (getdata json)]
                          (set-key-value :mutations (.-data data))
                          (set-key-value :total-pages
                                         (get-total-rec-no (.-pagesCount data)))
                          (js/console.log (get-value! :total-pages))
                                                    (set-key-value :page-location
                                         [render-mutations (get-value! :mutations)])))]
    (set-key-value :current-page 1)
    (set-key-value :is-searched-results true)
    (http-get (str (get-search-url mn dt) "&pageIndex=0&pageSize=10")
              onres)))


(defn form-validator [data-set]
  (first (b/validate data-set
                     :mutationnumber [[v/required :message "Field is required"]]
                     :nameofthefirstparty [[v/required :message "Field is required"]]
                     :nameofthesecondparty [[v/required :message "Field is required"]]
                     :dateofinstitution [[v/required :message "Field is required"]]
                     :nameofpo [[v/required :message "Field is required"]]
                     :dateofdecision [[v/required :message "Field is required"]]
                     :title [[v/required :message "Field is required"]]
                     :khasranumber [[v/required :message "Field is required"]]
                     :area [[v/required :message "Field is required"]]
                     :khatakhatuninumber [[v/required :message "Field is required"]]
                     :subdivisionname  [[v/required :message "Field is required"]]
                     :o2number [[v/required :message "Field is required"]]
                     :o4number [[v/required :message "Field is required"]]
                     :o6number [[v/required :message "Field is required"]]
                     :racknumber [[v/required :message "Field is required"]]
                     :receiveddate [[v/required :message "Field is required"]]
                     :remarks [[v/required :message "Field is required"]])))

(defn form-input-element [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-element id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((form-validator @data-set) id)))]])
                        [:div])]])))


(defn add-form-onclick [data-set focus]
  (if (= nil (form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid
                                (int (.-value (.getElementById js/document "districts")))))
        (let [onres (fn[json]
                      (secretary/dispatch! "/"))]
          (http-post (str serverhost "mutations")
                     onres  (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on")))


(defn update-form-onclick [data-set focus]
  (if (= nil (form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid
                                (int (.-value (.getElementById js/document "districts")))))
        (let [onres (fn[data]
                      (secretary/dispatch! "/"))]
          (http-put (str serverhost "mutations/" (:id @data-set))
                    onres (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on")))

(defn form-cancel [event]
  (secretary/dispatch! "/"))

(defn on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 ;; (let [dt  (aggre-str (getdata json))]
                 ;;   (set! (.-value ele) (if(empty? dt) eval dt))))]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]

    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get (str serverhost "villages/search?name=" eval)
                onresp))))

(defn input [label type id]
  (row label [:input.form-control {:type type :id id :on-change on-change}]))

(defn get-villages []
  (let [onres (fn[json]((set-key-value :villages (getdata json))))]
    (http-get (str serverhost "villages") onres)))

(defn tags-template [data-set]
  (cond (nil? (:villageid @data-set)) [:select.form-control {:id "districts"}
                                       (for [d (get-value! :villages)]
                                         ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)])]
        :else [:select.form-control {:id "districts" :defaultValue (:villageid @data-set)}
               (doall (for [d (get-value! :villages)]
                        ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)]))]))


(defn form-input-select [label data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div#tagdiv.col-sm-6 [tags-template data-set]]
       [:div.col-sm-3 [:div]]])))

(defn datalist []
  [:datalist {:id "combo"}
   (let [name-po ["Kumar" "Sai" "Bhaskar" "Rajesh"]]
     (for [i name-po]
       ^{:key i}
       [:option {:value i}]))])

(defn input-combo [id data-set placeholder in-focus]
  [:input.form-control {:id id
                        :list "combo"
                        :value (@data-set id)
                        :placeholder placeholder
                        :on-change #(swap! data-set assoc id (-> % .-target .-value))
                        :on-blur  #(reset! in-focus "on")
                        } [datalist ]])

(defn form-input-combo [id label data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-combo id data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((form-validator @data-set) id)))]])
                        [:div])]])))

(defn mutation-template [doc-name data-set focus save-function]
 [:div.container
  [:div.col-md-12
   [:div.box.box-info
    [:div.box-header.with-border 
     [:h2.box-title doc-name]]
    [:div.form-horizontal
     [:div.box-body
       [form-input-element :mutationnumber "Mutation Number" "text" data-set focus]
       [form-input-element :nameofthefirstparty "Name of the First Party" "text" data-set focus ]
       [form-input-element :nameofthesecondparty "Name of the Second Party" "text" data-set focus]
       [form-input-element :dateofinstitution "Date of Institution" "date" data-set focus]
       [form-input-combo   :nameofpo "Name of PO" data-set focus]
       [form-input-element :dateofdecision "Date of Decision" "date" data-set focus]
       [form-input-element :title "Title" "text" data-set focus]
       [form-input-element :khasranumber "Khasra Number" "text" data-set focus]
       [form-input-element :area "Area" "text" data-set focus]
       [form-input-element :khatakhatuninumber "Khata Khatuni Number" "text" data-set focus]
       [form-input-select "Village Name" data-set focus]
       [form-input-element :subdivisionname "Sub division Name" "text" data-set focus]
       [form-input-element :o2number "O2 Number" "text" data-set focus]
       [form-input-element :o4number "O4 Number" "text" data-set focus]
       [form-input-element :o6number "O6 Number" "text" data-set focus]
       [form-input-element :racknumber "Rack Number" "text" data-set focus]
       [form-input-element :receiveddate "Received Date" "date" data-set focus]
       [form-input-element :remarks "Remarks" "text" data-set focus]]
     [:div.box-footer
      [:button.btn.btn-default {:on-click form-cancel} "Cancel"]
      [:button.btn.btn-info.pull-right {:on-click save-function} "Save"]]]]]])

(defn mutation-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [mutation-template "Mutation Add Form" add-data focus #(add-form-onclick add-data focus)])))

(defn mutation-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :mutationnumber (.-mutationnumber dmt)
                             :nameofthefirstparty (.-nameofthefirstparty dmt)
                             :nameofthesecondparty (.-nameofthesecondparty dmt)
                             :dateofinstitution (.-dateofinstitution dmt)
                             :nameofpo (.-nameofpo dmt)
                             :dateofdecision (.-dateofdecision dmt)
                             :title (.-title dmt)
                             :khasranumber (.-khasranumber dmt)
                             :khatakhatuninumber (.-khasranumber dmt)
                             :area (.-area dmt)
                             :subdivisionname (.-subdivisionname dmt)
                             :villageid (.-villageid dmt)
                             :o2number (.-o2number dmt)
                             :o4number (.-o4number dmt)
                             :o6number (.-o6number dmt)
                             :racknumber (.-racknumber dmt)
                             :receiveddate (.-receiveddate dmt)
                             :remarks (.-remarks dmt)
                             :villagename (.-villagename dmt)
                             :districtname (.-districtname dmt)
                             :isactive true})
        focus (r/atom nil)]
    (fn [] [mutation-template
            "Mutation Update Form"
            update-data
            focus
            #(update-form-onclick update-data focus)])))


(defn click-update[id]
  (secretary/dispatch! (str "/mutations/update/" id)))

(defn delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/"))]
    (http-delete (str serverhost "mutations/" id)  onres)))

(defn add [event]
  (secretary/dispatch! "/mutations/add"))

(defn get-all-click [event]
  (let [onres (fn [json]
                (let [mt (getdata json)]
                  (set-key-value :mutations (.-data mt))
                  (set-key-value :total-pages (get-total-rec-no
                                               (.-pagesCount mt)))
                  (set-key-value :current-page 1)
                  (set-key-value :page-location
                                 [render-mutations (get-value! :mutations)])))]
    (set-key-value :is-searched-results false)
    (http-get (str serverhost "mutations?pageIndex=0&pageSize=10") onres)))

(defn render-mutations [mutations]
  [:div
   ;; [:div.padding]
   ;; [:div.page-header [:h1 "Record Room Management System"]]
   [:div#add]
   [:div#update]
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of Mutations"]]
      [:div.col-md-12
       ;; [:div.col-sm-2 [:input.form-control {:id "dt1" :type "date"}]]
       ;; [:div.col-sm-2 [:input.form-control {:id "dt2" :type "date"}]]
       [:div.form-group
        [:div.row
         [:div.col-sm-2 "Mutation Number"
          [:input.form-control {:id "mutationnumber"
                              :type "text"
                                :placeholder "Enter search by mutationnumber"}]]
         [:div.col-sm-2 "Enter Search Text"
          [:input.form-control {:id "dt" :type "text"
                                               :placeholder "Enter search text.."}]]]]
       [:div.form-group
        [:div.row
         [:div.col-sm-2 "Title"
          [:input.form-control {:id "stitle"
                                :type "text"
                                :placeholder "Title"}]]
         [:div.col-sm-2 "District Name"
          [:input.form-control {:id "sdistrictname"
                                :list "combo"
                                :placeholder "District Name"}[datalist] ]]
         [:div.col-sm-2 "Sub Division Name"
          [:input.form-control {:id "ssubdivisionname"
                                :list "combo"
                                :placeholder "Sub Division Name"}[datalist]]]
         [:div.col-sm-2 "Village Name"
          [:input.form-control {:id "svillagename"
                                :list "combo"
                                :placeholder "Village Name"}[datalist]]]
         [:div.col-sm-2 "Name Of P.O"
          [:input.form-control {:id "svillagename"
                                :list "combo"
                                :placeholder "Name Of P.O"}[datalist]]]]]
       [:div.form-group
        [:div.row
         [:div.col-sm-2 "O2 Number"
          [:input.form-control {:id "so2number"
                                :type "text"
                                :placeholder "O2 Number"}]]
         [:div.col-sm-2 "Khasra Number"
          [:input.form-control {:id "skhasranumber"
                                :type "text"
                                :placeholder "Khasra Number"} ]]
         [:div.col-sm-2 "Khata khatuni Number"
          [:input.form-control {:id "skhatakhatuninumber"
                                :type "text"
                                :placeholder "Khata Khatuni Number"}]]
         [:div.col-sm-2 "Name of the First Party"
          [:input.form-control {:id "snameofthefirstparty"
                                :type "text"
                                :placeholder "Name of the First Party"}]]
         [:div.col-sm-2 "Name of the Second Party"
          [:input.form-control {:id "snameofthesecondparty"
                                :type "text"
                                :placeholder "Name of the Second Party"}]]]
        [:div.form-group
         [:div.row
          [:div.col-sm-2.col-md-offset-5 {:style {:padding-top "20px"}}
           [:input.btn.btn-primary {:type "button" :value "Search" :on-click search}]
           [:input.btn.btn-primary {:type "button" :value "Add" :on-click add}]
           [:input.btn.btn-primary {:id "getall" :type "button" :value "Refresh":on-click get-all-click}]]]]]]

    [:div {:class "box-body"}
     [:table {:class "table table-bordered table-striped dataTable"}
      [:thead
       [:tr
        [:th "Mutation Number"]
        [:th "Name of the FirstParty"]
        [:th "Name of The SecondParty"]
        [:th "Date of Institution"]
        [:th "Name of P.O"]
        [:th "Name of Districts"]
        [:th "Name of Village"]
        [:th "SubDivisionName"]
        [:th " "]
        [:th " "]
        ]]
      [:tbody
       (for [mt mutations]
         ^{:key (.-id mt)} [:tr
                            [:td (.-mutationnumber mt)]
                            [:td (.-nameofthefirstparty mt)]
                            [:td (.-nameofthesecondparty mt)]
                            ;;  [:td  (f/unparse (f/formatter "dd-MMM-yyyy")(f/parse (.-dateofinstitution dn)))]
                            [:td (.-dateofinstitution mt)]
                            [:td (.-nameofpo mt)]
                            [:td (.-districtname mt)]
                            [:td (.-villagename mt)]
                            [:td (.-subdivisionname mt)]
                            ;; [:td [:input {:type "button" :on-click #(click-update(.-id dn))
                            ;;               :class "glyphicon glyphicon-edit" :value "Update"}
                            ;;       ]]
                            [:td [:a {:href "javascript:;" :on-click  #(click-update (.-id mt))  :class "btn btn-success btn-sm glyphicon glyphicon-edit"}]]
                            ;; [:td [:input {:type "button" :on-click #(delete(.-id dn))
                            ;;               :class "glyphicon glyphicon-remove"  :value "Delete"}]]
                            [:td  [:a {:href "javascript:;" :on-click #(delete(.-id mt))  :class "btn btn-danger btn-sm glyphicon glyphicon-remove"}] ]])]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]])

(defroute mutations-list "/mutations" []
  (let [onres (fn [json]
                (let [dt (getdata json)]
                  (set-key-value :mutations (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-key-value :page-location  [render-mutations (get-value! :mutations)])))]
    (set-key-value :is-searched-results false)
    (http-get (str serverhost "mutations?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


(defroute documents-path "/mutations/add" []
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [mutation-add-template])))]
    (http-get (str serverhost "villages") onres)))

(defroute documents-path1 "/mutations/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [mutation-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :mutations)))])))]
    (http-get (str serverhost "villages") onres)))

;; ---------------------------------------------------------
;; rvenue-records

(defn revenue-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber [[v/required :message "Field is required Must be number"]]
                     :subdivisionname [[v/required :message "Field is required"]]
                     :tehsil [[v/required :message "Field is required"]]
                     :year [[v/required :message "Field is required"]]
                     :racknumber [[v/required :message "Field is required"]]
                     :description [[v/required :message "Field is required"]])))


(defn input-int [id ttype data-set placeholder in-focus]
  [:input.form-control {:id id
                        :type ttype
                        :value (@data-set id)
                        :placeholder placeholder
                        :on-change #(swap! data-set assoc id (int (-> % .-target .-value )))
                        :on-blur  #(reset! in-focus "on")
                        }])


(defn revenue-input-int-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-int id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (revenue-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((revenue-form-validator @data-set) id)))]])
                        [:div])]])))

(defn revenue-input-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-element id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (revenue-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((revenue-form-validator @data-set) id)))]])
                        [:div])]])))


(defn revenue-template [doc-name data-set focus save-function]
 [:div.container
  [:div.col-md-12
   [:div.box.box-info
    [:div.box-header.with-border 
     [:h2.box-title doc-name]]
    [:div.form-horizontal
     [:div.box-body
       [revenue-input-int-row :serialnumber "Serial Number" "text" data-set focus]
       [revenue-input-select "Village Name" data-set focus ]
       [revenue-input-row :subdivisionname "Sub Division Name" "text" data-set focus]
       [revenue-input-row :tehsil "Tehsil" "text" data-set focus]
       [revenue-input-row :year "Year" "date" data-set focus]
       [revenue-input-row :racknumber "Rack Number" "text" data-set focus]
       [revenue-input-row :description "description" "text" data-set focus]
    [:div.box-footer
      [:button.btn.btn-default {:on-click revenue-form-cancel} "Cancel"]
      [:button.btn.btn-info.pull-right {:on-click save-function} "Save"]]]]]]])


(defn revenue-add-form-onclick [data-set focus]
  (if (= nil (revenue-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid (int (.-value (.getElementById js/document "revenue-districts")))))
        (let [onres (fn[json] (secretary/dispatch! "/revenue"))]
          (http-post (str serverhost "revenuerecords") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on")))


(defn revenue-update-form-onclick [data-set focus]
  (if (= nil (revenue-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid (int (.-value (.getElementById js/document "revenue-districts")))))
        (let [onres (fn[data] (secretary/dispatch! "/revenue"))]
          (http-put (str serverhost "revenuerecords/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on")))

(defn revenue-form-cancel [event]
  (secretary/dispatch! "/revenue"))

(defn revenue-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get (str serverhost "villages/search?name=" eval) onresp))))


(defn revenue-tags-template [data-set]
  (cond (nil? (:villageid @data-set)) [:select.form-control {:id "revenue-districts"}
                                       (for [d (get-value! :villages)]
                                         ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)])]
        :else [:select.form-control {:id "revenue-districts" :defaultValue (:villageid @data-set)}
               (doall (for [d (get-value! :villages)]
                        ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)]))]))

(defn revenue-input-select [label data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div#tagdiv.col-sm-6 [revenue-tags-template data-set]]
       [:div.col-sm-3 [:div]]])))


(defn revenue-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [revenue-template 
    	    "Revenue Add Form" 
    	      add-data focus
    	       #(revenue-add-form-onclick add-data focus)])))

(defn revenue-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :serialnumber (.-serialnumber dmt)
                             :subdivisionname (.-subdivisionname dmt)
                             :villageid (.-villageid dmt)
                             :villagename (.-villagename dmt)
                             :tehsil (.-tehsil dmt)
                             :year (.-year dmt)
                             :racknumber (.-racknumber dmt)
                             :description (.-description dmt)
                             :isactive true})
        focus (r/atom nil)]
    (fn [] [revenue-template
            "Revenue Update Form"
            update-data focus
            #(revenue-update-form-onclick update-data focus)])))


(defn revenue-update[id]
  (secretary/dispatch! (str "/revenue/update/" id)))

(defn revenue-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/revenue"))]
    (http-delete (str serverhost "revenuerecords/" id)  onres)))


(defroute revenue-list "/revenue" []
  (let [onres (fn [json]
                (let [dt (getdata json)]
                  (set-key-value :revenues (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-key-value :page-location  [render-revenue (get-value! :revenues)])))]
    (set-key-value :is-searched-results false)
    (http-get (str serverhost "revenuerecords?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


(defroute revenue-add-path "/revenue/add" []
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [revenue-add-template])))]
    (http-get (str serverhost "villages") onres)))

(defroute revenue-upd-path "/revenue/update/:id" [id]
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [revenue-update-template id
                                    (first (filter (fn[obj]
                                                     (=(.-id obj) (.parseInt js/window id))) (get-value! :revenues)))])))]
    (http-get (str serverhost "villages") onres)))

(defn revenue-add [event]
  (secretary/dispatch! "/revenue/add"))

(defn render-revenue [revenues]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of Revenue Records"]]
    [:div.row
     [:div.col-md-12
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click revenue-add}] 
       ;; [:input {:id "getall" :type "button" :value "Refresh"
       ;;          :class "btn btn-primary" :on-click get-all-click}]
       ]
      [:div {:class "box-body"}

       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          [:th "S.No"]
          [:th "Name of the Village"]
          [:th "Sub Division Name"]
          [:th "Tehsil"]
          [:th "Year"]
          [:th "Rack Number"]
          [:th "Description"]
          [:th " "]
          [:th " "]
          ]]
        [:tbody
         (for [mt revenues]
           ^{:key (.-id mt)} [:tr
                              [:td (.-serialnumber mt)]
                              [:td (.-villagename mt)]
                              [:td (.-subdivisionname mt)]
                              [:td (.-tehsil mt)]
                              [:td (.-year mt)]
                              [:td (.-racknumber mt)]
                              [:td (.-description mt)]
                              [:td [:a {:href "javascript:;"
                                        :on-click  #(revenue-update(.-id mt))
                                        :class "btn btn-success btn-sm glyphicon glyphicon-edit"}]]
                              [:td  [:a {:href "javascript:;" :on-click #(revenue-delete(.-id mt))
                                         :class "btn btn-danger btn-sm glyphicon glyphicon-remove"}]]])]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])


;; ---------------------------------------------------------
;; khasragirdwani-records

(defn khasragirdwani-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber [[v/required :message "Field is required Must be number"]]
                     :subdivisionname [[v/required :message "Field is required"]]
                     :tehsil [[v/required :message "Field is required"]]
                     :year [[v/required :message "Field is required"]]
                     :racknumber [[v/required :message "Field is required"]]
                     :description [[v/required :message "Field is required"]])))



(defn khasragirdwani-input-int-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-int id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (khasragirdwani-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((khasragirdwani-form-validator @data-set) id)))]])
                        [:div])]])))

(defn khasragirdwani-input-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-element id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (khasragirdwani-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((khasragirdwani-form-validator @data-set) id)))]])
                        [:div])]])))


(defn khasragirdwani-template [doc-name data-set focus save-function]
 [:div.container
  [:div.col-md-12
   [:div.box.box-info
    [:div.box-header.with-border 
     [:h2.box-title doc-name]]
    [:div.form-horizontal
     [:div.box-body
       [khasragirdwani-input-int-row :serialnumber "Serial Number" "text" data-set focus]
       [khasragirdwani-input-select "Village Name" data-set focus ]
       [khasragirdwani-input-row :subdivisionname "Sub Division Name" "text" data-set focus]
       [khasragirdwani-input-row :tehsil "Tehsil" "text" data-set focus]
       [khasragirdwani-input-row :year "Year" "date" data-set focus]
       [khasragirdwani-input-row :racknumber "Rack Number" "text" data-set focus]
       [khasragirdwani-input-row :description "description" "text" data-set focus]
    [:div.box-footer
      [:button.btn.btn-default {:on-click khasragirdwani-form-cancel} "Cancel"]
      [:button.btn.btn-info.pull-right {:on-click save-function} "Save"]]]]]]])


(defn khasragirdwani-add-form-onclick [data-set focus]
  (if (= nil (khasragirdwani-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid
                                (int (.-value (.getElementById js/document "khasragirdwani-districts")))))
        (let [onres (fn[json] (secretary/dispatch! "/khasragirdwani"))]
          (http-post (str serverhost "khasragirdwanis") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on")))


(defn khasragirdwani-update-form-onclick [data-set focus]
  (if (= nil (khasragirdwani-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid
                                (int (.-value (.getElementById js/document "khasragirdwani-districts")))))
        (let [onres (fn[data] (secretary/dispatch! "/khasragirdwani"))]
          (http-put (str serverhost "khasragirdwanis/"
                         (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on")))

(defn khasragirdwani-form-cancel [event]
  (secretary/dispatch! "/khasragirdwani"))

(defn khasragirdwani-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get (str serverhost "villages/search?name=" eval) onresp))))


(defn khasragirdwani-tags-template [data-set]
  (cond (nil? (:villageid @data-set)) [:select.form-control {:id "khasragirdwani-districts"}
                                       (for [d (get-value! :villages)]
                                         ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)])]
        :else [:select.form-control {:id "khasragirdwani-districts" :defaultValue (:villageid @data-set)}
               (doall (for [d (get-value! :villages)]
                        ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)]))]))

(defn khasragirdwani-input-select [label data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div#tagdiv.col-sm-6 [khasragirdwani-tags-template data-set]]
       [:div.col-sm-3 [:div]]])))


(defn khasragirdwani-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [khasragirdwani-template 
          "khasragirdwani Add Form" 
            add-data focus
             #(khasragirdwani-add-form-onclick add-data focus)])))

(defn khasragirdwani-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :serialnumber (.-serialnumber dmt)
                             :subdivisionname (.-subdivisionname dmt)
                             :villageid (.-villageid dmt)
                             :villagename (.-villagename dmt)
                             :tehsil (.-tehsil dmt)
                             :year (.-year dmt)
                             :racknumber (.-racknumber dmt)
                             :description (.-description dmt)
                             :isactive true})
        focus (r/atom nil)]
    (fn [] [khasragirdwani-template
            "khasragirdwani Update Form"
            update-data focus
            #(khasragirdwani-update-form-onclick update-data focus)])))


(defn khasragirdwani-update[id]
  (secretary/dispatch! (str "/khasragirdwani/update/" id)))

(defn khasragirdwani-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/khasragirdwani"))]
    (http-delete (str serverhost "khasragirdwanis/" id)  onres)))


(defroute khasragirdwani-list "/khasragirdwani" []
  (let [onres (fn [json]
                (let [dt (getdata json)]
                  (set-key-value :khasragirdwanis (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-key-value :page-location  [render-khasragirdwani (get-value! :khasragirdwanis)])))]
    (set-key-value :is-searched-results false)
    (http-get (str serverhost "khasragirdwanis?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


(defroute khasragirdwani-add-path "/khasragirdwani/add" []
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [khasragirdwani-add-template])))]
    (http-get (str serverhost "villages") onres)))

(defroute khasragirdwani-upd-path "/khasragirdwani/update/:id" [id]
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [khasragirdwani-update-template id
                                    (first (filter (fn[obj]
                                                     (=(.-id obj) (.parseInt js/window id)))
                                                   (get-value! :khasragirdwanis)))])))]
    (http-get (str serverhost "villages") onres)))

(defn khasragirdwani-add [event]
  (secretary/dispatch! "/khasragirdwani/add"))

(defn render-khasragirdwani [khasragirdwanis]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of khasragirdwani Records"]]
    [:div.row
     [:div.col-md-12
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click khasragirdwani-add}] 
       ;; [:input {:id "getall" :type "button" :value "Refresh"
       ;;          :class "btn btn-primary" :on-click get-all-click}]
       ]
      [:div {:class "box-body"}

       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          [:th "S.No"]
          [:th "Name of the Village"]
          [:th "Sub Division Name"]
          [:th "Tehsil"]
          [:th "Year"]
          [:th "Rack Number"]
          [:th "Description"]
          [:th " "]
          [:th " "]
          ]]
        [:tbody
         (for [mt khasragirdwanis]
           ^{:key (.-id mt)} [:tr
                              [:td (.-serialnumber mt)]
                              [:td (.-villagename mt)]
                              [:td (.-subdivisionname mt)]
                              [:td (.-tehsil mt)]
                              [:td (.-year mt)]
                              [:td (.-racknumber mt)]
                              [:td (.-description mt)]
                              [:td [:a {:href "javascript:;"
                                        :on-click  #(khasragirdwani-update(.-id mt))
                                        :class "btn btn-success btn-sm glyphicon glyphicon-edit"}]]
                              [:td  [:a {:href "javascript:;" :on-click #(khasragirdwani-delete(.-id mt))
                                         :class "btn btn-danger btn-sm glyphicon glyphicon-remove"}]]])]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])

;; ---------------------------------------------------------
;; masavi-records

(defn masavi-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber [[v/required :message "Field is required Must be number"]]
                     :subdivisionname [[v/required :message "Field is required"]]
                     :tehsil [[v/required :message "Field is required"]]
                     :year [[v/required :message "Field is required"]]
                     :racknumber [[v/required :message "Field is required"]]
                     :description [[v/required :message "Field is required"]])))



(defn masavi-input-int-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-int id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (masavi-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((masavi-form-validator @data-set) id)))]])
                        [:div])]])))

(defn masavi-input-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-element id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (masavi-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((masavi-form-validator @data-set) id)))]])
                        [:div])]])))


(defn masavi-template [doc-name data-set focus save-function]
 [:div.container
  [:div.col-md-12
   [:div.box.box-info
    [:div.box-header.with-border 
     [:h2.box-title doc-name]]
    [:div.form-horizontal
     [:div.box-body
       [masavi-input-int-row :serialnumber "Serial Number" "text" data-set focus]
       [masavi-input-select "Village Name" data-set focus ]
       [masavi-input-row :subdivisionname "Sub Division Name" "text" data-set focus]
       [masavi-input-row :tehsil "Tehsil" "text" data-set focus]
       [masavi-input-row :year "Year" "date" data-set focus]
       [masavi-input-row :racknumber "Rack Number" "text" data-set focus]
       [masavi-input-row :description "description" "text" data-set focus]
    [:div.box-footer
      [:button.btn.btn-default {:on-click masavi-form-cancel} "Cancel"]
      [:button.btn.btn-info.pull-right {:on-click save-function} "Save"]]]]]]])


(defn masavi-add-form-onclick [data-set focus]
  (if (= nil (masavi-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid (int (.-value (.getElementById js/document "masavi-districts")))))
        (let [onres (fn[json] (secretary/dispatch! "/masavi"))]
          (http-post (str serverhost "masavis") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on")))


(defn masavi-update-form-onclick [data-set focus]
  (if (= nil (masavi-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid (int (.-value (.getElementById js/document "masavi-districts")))))
        (let [onres (fn[data] (secretary/dispatch! "/masavi"))]
          (http-put (str serverhost "masavis/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on")))

(defn masavi-form-cancel [event]
  (secretary/dispatch! "/masavi"))

(defn masavi-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get (str serverhost "villages/search?name=" eval) onresp))))


(defn masavi-tags-template [data-set]
  (cond (nil? (:villageid @data-set)) [:select.form-control {:id "masavi-districts"}
                                       (for [d (get-value! :villages)]
                                         ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)])]
        :else [:select.form-control {:id "masavi-districts" :defaultValue (:villageid @data-set)}
               (doall (for [d (get-value! :villages)]
                        ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)]))]))

(defn masavi-input-select [label data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div#tagdiv.col-sm-6 [masavi-tags-template data-set]]
       [:div.col-sm-3 [:div]]])))


(defn masavi-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [masavi-template 
          "masavi Add Form" 
            add-data focus
             #(masavi-add-form-onclick add-data focus)])))

(defn masavi-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :serialnumber (.-serialnumber dmt)
                             :subdivisionname (.-subdivisionname dmt)
                             :villageid (.-villageid dmt)
                             :villagename (.-villagename dmt)
                             :tehsil (.-tehsil dmt)
                             :year (.-year dmt)
                             :racknumber (.-racknumber dmt)
                             :description (.-description dmt)})
        focus (r/atom nil)]
    (fn [] [masavi-template
            "masavi Update Form"
            update-data focus
            #(masavi-update-form-onclick update-data focus)])))


(defn masavi-update[id]
  (secretary/dispatch! (str "/masavi/update/" id)))

(defn masavi-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/masavi"))]
    (http-delete (str serverhost "masavis/" id)  onres)))


(defroute masavi-list "/masavi" []
  (let [onres (fn [json]
                (let [dt (getdata json)]
                  (set-key-value :masavis (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-key-value :page-location  [render-masavi (get-value! :masavis)])))]
    (set-key-value :is-searched-results false)
    (http-get (str serverhost "masavis?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


(defroute masavi-add-path "/masavi/add" []
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [masavi-add-template])))]
    (http-get (str serverhost "villages") onres)))

(defroute masavi-upd-path "/masavi/update/:id" [id]
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [masavi-update-template id
                                    (first (filter (fn[obj]
                                                     (=(.-id obj) (.parseInt js/window id))) (get-value! :masavis)))])))]
    (http-get (str serverhost "villages") onres)))

(defn masavi-add [event]
  (secretary/dispatch! "/masavi/add"))

(defn render-masavi [masavis]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of masavi Records"]]
    [:div.row
     [:div.col-md-12
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click masavi-add}] 
       ;; [:input {:id "getall" :type "button" :value "Refresh"
       ;;          :class "btn btn-primary" :on-click get-all-click}]
       ]
      [:div {:class "box-body"}

       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          [:th "S.No"]
          [:th "Name of the Village"]
          [:th "Sub Division Name"]
          [:th "Tehsil"]
          [:th "Year"]
          [:th "Rack Number"]
          [:th "Description"]
          [:th " "]
          [:th " "]
          ]]
        [:tbody
         (for [mt masavis]
           ^{:key (.-id mt)} [:tr
                              [:td (.-serialnumber mt)]
                              [:td (.-villagename mt)]
                              [:td (.-subdivisionname mt)]
                              [:td (.-tehsil mt)]
                              [:td (.-year mt)]
                              [:td (.-racknumber mt)]
                              [:td (.-description mt)]
                              [:td [:a {:href "javascript:;"
                                        :on-click  #(masavi-update(.-id mt))
                                        :class "btn btn-success btn-sm glyphicon glyphicon-edit"}]]
                              [:td  [:a {:href "javascript:;" :on-click #(masavi-delete(.-id mt))
                                         :class "btn btn-danger btn-sm glyphicon glyphicon-remove"}]]])]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])


;; ---------------------------------------------------------
;; Consolidation-records

(defn consolidation-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber [[v/required :message "Field is required Must be number"]]
                     :subdivisionname [[v/required :message "Field is required"]]
                     :tehsil [[v/required :message "Field is required"]]
                     :year [[v/required :message "Field is required"]]
                     :racknumber [[v/required :message "Field is required"]]
                     :description [[v/required :message "Field is required"]])))



(defn consolidation-input-int-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-int id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (consolidation-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((consolidation-form-validator @data-set) id)))]])
                        [:div])]])))

(defn consolidation-input-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-element id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (consolidation-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((consolidation-form-validator @data-set) id)))]])
                        [:div])]])))


(defn consolidation-template [doc-name data-set focus save-function]
 [:div.container
  [:div.col-md-12
   [:div.box.box-info
    [:div.box-header.with-border 
     [:h2.box-title doc-name]]
    [:div.form-horizontal
     [:div.box-body
       [consolidation-input-int-row :serialnumber "Serial Number" "text" data-set focus]
       [consolidation-input-select "Village Name" data-set focus ]
       [consolidation-input-row :subdivisionname "Sub Division Name" "text" data-set focus]
       [consolidation-input-row :tehsil "Tehsil" "text" data-set focus]
       [consolidation-input-row :year "Year" "date" data-set focus]
       [consolidation-input-row :racknumber "Rack Number" "text" data-set focus]
       [consolidation-input-row :description "description" "text" data-set focus]
    [:div.box-footer
      [:button.btn.btn-default {:on-click consolidation-form-cancel} "Cancel"]
      [:button.btn.btn-info.pull-right {:on-click save-function} "Save"]]]]]]])


(defn consolidation-add-form-onclick [data-set focus]
  (if (= nil (consolidation-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid (int (.-value (.getElementById js/document "consolidation-districts")))))
        (let [onres (fn[json] (secretary/dispatch! "/consolidation"))]
          (http-post (str serverhost "consolidations") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on")))


(defn consolidation-update-form-onclick [data-set focus]
  (if (= nil (consolidation-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid (int (.-value (.getElementById js/document "consolidation-districts")))))
        (let [onres (fn[data] (secretary/dispatch! "/consolidation"))]
          (http-put (str serverhost "consolidations/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on")))

(defn consolidation-form-cancel [event]
  (secretary/dispatch! "/consolidation"))

(defn consolidation-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get (str serverhost "villages/search?name=" eval) onresp))))


(defn consolidation-tags-template [data-set]
  (cond (nil? (:villageid @data-set)) [:select.form-control {:id "consolidation-districts"}
                                       (for [d (get-value! :villages)]
                                         ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)])]
        :else [:select.form-control {:id "consolidation-districts" :defaultValue (:villageid @data-set)}
               (doall (for [d (get-value! :villages)]
                        ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)]))]))

(defn consolidation-input-select [label data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div#tagdiv.col-sm-6 [consolidation-tags-template data-set]]
       [:div.col-sm-3 [:div]]])))


(defn consolidation-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [consolidation-template 
          "consolidation Add Form" 
            add-data focus
             #(consolidation-add-form-onclick add-data focus)])))

(defn consolidation-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :serialnumber (.-serialnumber dmt)
                             :subdivisionname (.-subdivisionname dmt)
                             :villageid (.-villageid dmt)
                             :villagename (.-villagename dmt)
                             :tehsil (.-tehsil dmt)
                             :year (.-year dmt)
                             :racknumber (.-racknumber dmt)
                             :description (.-description dmt)})
        focus (r/atom nil)]
    (fn [] [consolidation-template
            "consolidation Update Form"
            update-data focus
            #(consolidation-update-form-onclick update-data focus)])))


(defn consolidation-update[id]
  (secretary/dispatch! (str "/consolidation/update/" id)))

(defn consolidation-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/consolidation"))]
    (http-delete (str serverhost "consolidations/" id)  onres)))


(defroute consolidation-list "/consolidation" []
  (let [onres (fn [json]
                (let [dt (getdata json)]
                  (set-key-value :consolidations (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-key-value :page-location  [render-consolidation (get-value! :consolidations)])))]
    (set-key-value :is-searched-results false)
    (http-get (str serverhost "consolidations?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


(defroute consolidation-add-path "/consolidation/add" []
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [consolidation-add-template])))]
    (http-get (str serverhost "villages") onres)))

(defroute consolidation-upd-path "/consolidation/update/:id" [id]
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [consolidation-update-template id
                                    (first (filter (fn[obj]
                                                     (=(.-id obj) (.parseInt js/window id))) (get-value! :consolidations)))])))]
    (http-get (str serverhost "villages") onres)))

(defn consolidation-add [event]
  (secretary/dispatch! "/consolidation/add"))

(defn render-consolidation [consolidations]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of consolidation Records"]]
    [:div.row
     [:div.col-md-12
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click consolidation-add}] 
       ;; [:input {:id "getall" :type "button" :value "Refresh"
       ;;          :class "btn btn-primary" :on-click get-all-click}]
       ]
      [:div {:class "box-body"}

       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          [:th "S.No"]
          [:th "Name of the Village"]
          [:th "Sub Division Name"]
          [:th "Tehsil"]
          [:th "Year"]
          [:th "Rack Number"]
          [:th "Description"]
          [:th " "]
          [:th " "]
          ]]
        [:tbody
         (for [mt consolidations]
           ^{:key (.-id mt)} [:tr
                              [:td (.-serialnumber mt)]
                              [:td (.-villagename mt)]
                              [:td (.-subdivisionname mt)]
                              [:td (.-tehsil mt)]
                              [:td (.-year mt)]
                              [:td (.-racknumber mt)]
                              [:td (.-description mt)]
                              [:td [:a {:href "javascript:;"
                                        :on-click  #(consolidation-update(.-id mt))
                                        :class "btn btn-success btn-sm glyphicon glyphicon-edit"}]]
                              [:td  [:a {:href "javascript:;" :on-click #(consolidation-delete(.-id mt))
                                         :class "btn btn-danger btn-sm glyphicon glyphicon-remove"}]]])]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])

;; ---------------------------------------------------------
;; fieldbook-records

(defn fieldbook-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber [[v/required :message "Field is required Must be number"]]
                     :subdivisionname [[v/required :message "Field is required"]]
                     :tehsil [[v/required :message "Field is required"]]
                     :year [[v/required :message "Field is required"]]
                     :racknumber [[v/required :message "Field is required"]]
                     :description [[v/required :message "Field is required"]])))



(defn fieldbook-input-int-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-int id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (fieldbook-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((fieldbook-form-validator @data-set) id)))]])
                        [:div])]])))

(defn fieldbook-input-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-element id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (fieldbook-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((fieldbook-form-validator @data-set) id)))]])
                        [:div])]])))


(defn fieldbook-template [doc-name data-set focus save-function]
 [:div.container
  [:div.col-md-12
   [:div.box.box-info
    [:div.box-header.with-border 
     [:h2.box-title doc-name]]
    [:div.form-horizontal
     [:div.box-body
       [fieldbook-input-int-row :serialnumber "Serial Number" "text" data-set focus]
       [fieldbook-input-select "Village Name" data-set focus ]
       [fieldbook-input-row :subdivisionname "Sub Division Name" "text" data-set focus]
       [fieldbook-input-row :tehsil "Tehsil" "text" data-set focus]
       [fieldbook-input-row :year "Year" "date" data-set focus]
       [fieldbook-input-row :racknumber "Rack Number" "text" data-set focus]
       [fieldbook-input-row :description "description" "text" data-set focus]
    [:div.box-footer
      [:button.btn.btn-default {:on-click fieldbook-form-cancel} "Cancel"]
      [:button.btn.btn-info.pull-right {:on-click save-function} "Save"]]]]]]])


(defn fieldbook-add-form-onclick [data-set focus]
  (if (= nil (fieldbook-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid (int (.-value (.getElementById js/document "fieldbook-districts")))))
        (let [onres (fn[json] (secretary/dispatch! "/fieldbook"))]
          (http-post (str serverhost "fieldbooks") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on")))


(defn fieldbook-update-form-onclick [data-set focus]
  (if (= nil (fieldbook-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid (int (.-value (.getElementById js/document "fieldbook-districts")))))
        (let [onres (fn[data] (secretary/dispatch! "/fieldbook"))]
          (http-put (str serverhost "fieldbooks/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on")))

(defn fieldbook-form-cancel [event]
  (secretary/dispatch! "/fieldbook"))

(defn fieldbook-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get (str serverhost "villages/search?name=" eval) onresp))))


(defn fieldbook-tags-template [data-set]
  (cond (nil? (:villageid @data-set)) [:select.form-control {:id "fieldbook-districts"}
                                       (for [d (get-value! :villages)]
                                         ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)])]
        :else [:select.form-control {:id "fieldbook-districts" :defaultValue (:villageid @data-set)}
               (doall (for [d (get-value! :villages)]
                        ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)]))]))

(defn fieldbook-input-select [label data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div#tagdiv.col-sm-6 [fieldbook-tags-template data-set]]
       [:div.col-sm-3 [:div]]])))


(defn fieldbook-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [fieldbook-template 
          "fieldbook Add Form" 
            add-data focus
             #(fieldbook-add-form-onclick add-data focus)])))

(defn fieldbook-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :serialnumber (.-serialnumber dmt)
                             :subdivisionname (.-subdivisionname dmt)
                             :villageid (.-villageid dmt)
                             :villagename (.-villagename dmt)
                             :tehsil (.-tehsil dmt)
                             :year (.-year dmt)
                             :racknumber (.-racknumber dmt)
                             :description (.-description dmt)})
        focus (r/atom nil)]
    (fn [] [fieldbook-template
            "fieldbook Update Form"
            update-data focus
            #(fieldbook-update-form-onclick update-data focus)])))


(defn fieldbook-update[id]
  (secretary/dispatch! (str "/fieldbook/update/" id)))

(defn fieldbook-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/fieldbook"))]
    (http-delete (str serverhost "fieldbooks/" id)  onres)))


(defroute fieldbook-list "/fieldbook" []
  (let [onres (fn [json]
                (let [dt (getdata json)]
                  (set-key-value :fieldbooks (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-key-value :page-location  [render-fieldbook (get-value! :fieldbooks)])))]
    (set-key-value :is-searched-results false)
    (http-get (str serverhost "fieldbooks?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


(defroute fieldbook-add-path "/fieldbook/add" []
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [fieldbook-add-template])))]
    (http-get (str serverhost "villages") onres)))

(defroute fieldbook-upd-path "/fieldbook/update/:id" [id]
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [fieldbook-update-template id
                                    (first (filter (fn[obj]
                                                     (=(.-id obj) (.parseInt js/window id))) (get-value! :fieldbooks)))])))]
    (http-get (str serverhost "villages") onres)))

(defn fieldbook-add [event]
  (secretary/dispatch! "/fieldbook/add"))

(defn render-fieldbook [fieldbooks]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of fieldbook Records"]]
    [:div.row
     [:div.col-md-12
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click fieldbook-add}] 
       ;; [:input {:id "getall" :type "button" :value "Refresh"
       ;;          :class "btn btn-primary" :on-click get-all-click}]
       ]
      [:div {:class "box-body"}

       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          [:th "S.No"]
          [:th "Name of the Village"]
          [:th "Sub Division Name"]
          [:th "Tehsil"]
          [:th "Year"]
          [:th "Rack Number"]
          [:th "Description"]
          [:th " "]
          [:th " "]
          ]]
        [:tbody
         (for [mt fieldbooks]
           ^{:key (.-id mt)} [:tr
                              [:td (.-serialnumber mt)]
                              [:td (.-villagename mt)]
                              [:td (.-subdivisionname mt)]
                              [:td (.-tehsil mt)]
                              [:td (.-year mt)]
                              [:td (.-racknumber mt)]
                              [:td (.-description mt)]
                              [:td [:a {:href "javascript:;"
                                        :on-click  #(fieldbook-update(.-id mt))
                                        :class "btn btn-success btn-sm glyphicon glyphicon-edit"}]]
                              [:td  [:a {:href "javascript:;" :on-click #(fieldbook-delete(.-id mt))
                                         :class "btn btn-danger btn-sm glyphicon glyphicon-remove"}]]])]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])

;; ---------------------------------------------------------
;; misc-records

(defn misc-form-validator [data-set]
  (first (b/validate data-set
                     :filenumber [[v/required :message "Field is required"]]
                     :subject [[v/required :message "Field is required"]]
                     :title [[v/required :message "Field is required"]]
                     :remarks [[v/required :message "Field is required"]]
                     :dispatcheddate [[v/required :message "Field is required"]]
                     :receiveddate [[v/required :message "Field is required"]]
                     )))


;; (defn misc-input-int-row [id label ttype data-set focus]
;;   (let [input-focus (r/atom nil)]
;;     (fn []
;;       [:div.form-group
;;        [:label.col-sm-3.control-label label]
;;        [:div.col-sm-6 [input-int id ttype data-set label input-focus]]
;;        [:div.col-sm-3 (if (or @input-focus @focus)
;;                         (if (= nil (misc-form-validator @data-set))
;;                           [:div]
;;                           [:div {:style  {:color "red"}}
;;                            [:b (str (first ((misc-form-validator @data-set) id)))]])
;;                         [:div])]])))

(defn misc-input-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-element id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (misc-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((misc-form-validator @data-set) id)))]])
                        [:div])]])))


(defn misc-template [doc-name data-set focus save-function]
 [:div.container
  [:div.col-md-12
   [:div.box.box-info
    [:div.box-header.with-border 
     [:h2.box-title doc-name]]
    [:div.form-horizontal
     [:div.box-body
       [misc-input-row :filenumber "File Numbar" "text" data-set focus]
       [misc-input-row :subject "Subject" "text" data-set focus]
       [misc-input-row :title "Title" "text" data-set focus]
       [misc-input-row :remarks "Remarks" "text" data-set focus]
       [misc-input-row :dispatcheddate "Dispatched Date" "date" data-set focus]
       [misc-input-row :receiveddate "Recevied Date" "date" data-set focus]
    [:div.box-footer
      [:button.btn.btn-default {:on-click misc-form-cancel} "Cancel"]
      [:button.btn.btn-info.pull-right {:on-click save-function} "Save"]]]]]]])

(defn misc-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [misc-template 
              "misc Add Form" 
               add-data
               focus
               #(misc-add-form-onclick add-data focus)])))

(defn misc-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :filenumber (.-filenumber dmt)
                             :subject (.-subject dmt)
                             :title (.-title dmt)
                             :remarks (.-remarks dmt)
                             :dispatcheddate (.-dispatcheddate dmt)
                             :receiveddate (.-receiveddate dmt)
                             })
        focus (r/atom nil)]
    (fn [] [misc-template
            "misc Update Form"
            update-data 
            focus
            #(misc-update-form-onclick update-data focus)])))


(defn misc-add-form-onclick [data-set focus]
  (if (= nil (misc-form-validator @data-set))
    (let [onres (fn[json] (secretary/dispatch! "/misc"))]
          (http-post (str serverhost "miscs") onres  (.serialize (Serializer.) (clj->js @data-set))))
  (reset! focus "on")))


(defn misc-update-form-onclick [data-set focus]
  (if (= nil (misc-form-validator @data-set))
      (let [onres (fn[data] (secretary/dispatch! "/misc"))]
          (http-put (str serverhost "miscs/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set))))
    (reset! focus "on")))

(defn misc-form-cancel [event]
  (secretary/dispatch! "/misc"))

(defn misc-add [event]
  (secretary/dispatch! "/misc/add"))

(defn misc-update[id]
  (secretary/dispatch! (str "/misc/update/" id)))

(defn misc-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/misc"))]
    (http-delete (str serverhost "miscs/" id)  onres)))


(defroute misc-list "/misc" []
  (let [onres (fn [json]
                (let [dt (getdata json)]
                  (set-key-value :miscs (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-key-value :page-location  [render-misc (get-value! :miscs)])))]
    (set-key-value :is-searched-results false)
    (http-get (str serverhost "miscs?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


(defroute misc-add-path "/misc/add" []
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [misc-add-template])))]
    (http-get (str serverhost "villages") onres)))

(defroute misc-upd-path "/misc/update/:id" [id]
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [misc-update-template id
                                    (first (filter (fn[obj]
                                                     (=(.-id obj) (.parseInt js/window id))) (get-value! :miscs)))])))]
    (http-get (str serverhost "villages") onres)))



(defn render-misc [miscs]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of misc Records"]]
    [:div.row
     [:div.col-md-12
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click misc-add}]
       ;; [:input {:id "getall" :type "button" :value "Refresh"
       ;;          :class "btn btn-primary" :on-click get-all-click}]
       ]
      [:div {:class "box-body"}

       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          [:th "Fille Number"]
          [:th "Subject"]
          [:th "Title"]
          [:th "Remarks"]
          [:th "Dispatched Date"]
          [:th "Received Date"]
          [:th " "]
          [:th " "]
          ]]
        [:tbody
         (for [mt miscs]
           ^{:key (.-id mt)} [:tr
                              [:td (.-filenumber mt)]
                              [:td (.-subject mt)]
                              [:td (.-title mt)]
                              [:td (.-remarks mt)]
                              [:td (.-dispatcheddate mt)]
                              [:td (.-receiveddate mt)]
                              [:td [:a {:href "javascript:;"
                                        :on-click  #(misc-update(.-id mt))
                                        :class "btn btn-success btn-sm glyphicon glyphicon-edit"}]]
                              [:td  [:a {:href "javascript:;" :on-click #(misc-delete(.-id mt))
                                         :class "btn btn-danger btn-sm glyphicon glyphicon-remove"}]]])]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])

;; ---------------------------------------------------------
;; o2register-records

(defn o2register-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber [[v/required :message "Field is required Must be number"]]
                     :tehsil [[v/required :message "Field is required"]]
                     :dateofinstitution [[v/required :message "Field is required"]]
                     :sourceofreceipt [[v/required :message "Field is required"]]
                     :nameofthefirstparty [[v/required :message "Field is required"]]
                     :dateofreceiptfrompanchayat [[v/required :message "Field is required"]]
                     :dateandgistoffinalorder [[v/required :message "Field is required"]]
                     :racknumber [[v/required :message "Field is required"]]
                     :startingyear [[v/required :message "Field is required Must be number"]]
                     :endingyear [[v/required :message "Field is required Must be number"]])))


(defn o2register-input-int-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-int id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (o2register-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((o2register-form-validator @data-set) id)))]])
                        [:div])]])))

(defn o2register-input-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-element id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (o2register-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((o2register-form-validator @data-set) id)))]])
                        [:div])]])))


(defn o2register-template [doc-name data-set focus save-function]
 [:div.container
  [:div.col-md-12
   [:div.box.box-info
    [:div.box-header.with-border 
     [:h2.box-title doc-name]]
    [:div.form-horizontal
     [:div.box-body
       [o2register-input-int-row :serialnumber "Serial Number" "text" data-set focus]
       [o2register-input-row :tehsil "Tehsil" "text" data-set focus]
       [o2register-input-row :dateofinstitution "Date Of Institution" "date" data-set focus]
       [o2register-input-row :sourceofreceipt "Source Of Receipt" "text" data-set focus]
       [o2register-input-select "Village Name" data-set focus ]
       [o2register-input-row :nameofthefirstparty "Name Of The First Party" "text" data-set focus]
       [o2register-input-row :dateofreceiptfrompanchayat "Date Of Receipt From Panchayat" "date" data-set focus]
       [o2register-input-row :dateandgistoffinalorder "Date and gist of Final Order" "date" data-set focus]
       [o2register-input-row :racknumber "Rack Number" "text" data-set focus]
       [o2register-input-int-row :startingyear "Starting Year" "text" data-set focus]
       [o2register-input-int-row :endingyear "Ending Year" "text" data-set focus]
    [:div.box-footer
      [:button.btn.btn-default {:on-click o2register-form-cancel} "Cancel"]
      [:button.btn.btn-info.pull-right {:on-click save-function} "Save"]]]]]]])


(defn o2register-add-form-onclick [data-set focus]
  (if (= nil (o2register-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid (int (.-value (.getElementById js/document "o2register-districts")))))
        (let [onres (fn[json] (secretary/dispatch! "/o2register"))]
          (http-post (str serverhost "o2registers") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on")))


(defn o2register-update-form-onclick [data-set focus]
  (if (= nil (o2register-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid (int (.-value (.getElementById js/document "o2register-districts")))))
        (let [onres (fn[data] (secretary/dispatch! "/o2register"))]
          (http-put (str serverhost "o2registers/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on")))

(defn o2register-form-cancel [event]
  (secretary/dispatch! "/o2register"))

(defn o2register-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get (str serverhost "villages/search?name=" eval) onresp))))


(defn o2register-tags-template [data-set]
  (cond (nil? (:villageid @data-set)) [:select.form-control {:id "o2register-districts"}
                                       (for [d (get-value! :villages)]
                                         ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)])]
        :else [:select.form-control {:id "o2register-districts" :defaultValue (:villageid @data-set)}
               (doall (for [d (get-value! :villages)]
                        ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)]))]))

(defn o2register-input-select [label data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div#tagdiv.col-sm-6 [o2register-tags-template data-set]]
       [:div.col-sm-3 [:div]]])))


(defn o2register-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [o2register-template 
          "o2register Add Form" 
            add-data focus
             #(o2register-add-form-onclick add-data focus)])))

(defn o2register-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :serialnumber (.-serialnumber dmt)
                             :dateofinstitution (.-dateofinstitution dmt)
                             :villageid (.-villageid dmt)
                             :villagename (.-villagename dmt)
                             :tehsil (.-tehsil dmt)
                             :sourceofreceipt (.-sourceofreceipt dmt)
                             :nameofthefirstparty (.-nameofthefirstparty dmt)
                             :dateofreceiptfrompanchayat (.-dateofreceiptfrompanchayat dmt)
                             :dateandgistoffinalorder (.-dateandgistoffinalorder dmt)
                             :racknumber (.-racknumber dmt)
                             :startingyear (.-startingyear dmt)
                             :endingyear (.-endingyear dmt)
                             })
        focus (r/atom nil)]
    (fn [] [o2register-template
            "o2register Update Form"
            update-data focus
            #(o2register-update-form-onclick update-data focus)])))


(defn o2register-update[id]
  (secretary/dispatch! (str "/o2register/update/" id)))

(defn o2register-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/o2register"))]
    (http-delete (str serverhost "o2registers/" id)  onres)))


(defroute o2register-list "/o2register" []
  (let [onres (fn [json]
                (let [dt (getdata json)]
                  (set-key-value :o2registers (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-key-value :page-location  [render-o2register (get-value! :o2registers)])))]
    (set-key-value :is-searched-results false)
    (http-get (str serverhost "o2registers?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


(defroute o2register-add-path "/o2register/add" []
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [o2register-add-template])))]
    (http-get (str serverhost "villages") onres)))

(defroute o2register-upd-path "/o2register/update/:id" [id]
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [o2register-update-template id
                                    (first (filter (fn[obj]
                                                     (=(.-id obj) (.parseInt js/window id))) (get-value! :o2registers)))])))]
    (http-get (str serverhost "villages") onres)))

(defn o2register-add [event]
  (secretary/dispatch! "/o2register/add"))

(defn render-o2register [o2registers]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of o2register Records"]]
    [:div.row
     [:div.col-md-12
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click o2register-add}] 
       ;; [:input {:id "getall" :type "button" :value "Refresh"
       ;;          :class "btn btn-primary" :on-click get-all-click}]
       ]
      [:div {:class "box-body"}

       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          [:th "S.No"]
          [:th "Tehsil"]
          [:th "Date of Institution"]
          [:th "Source of Receipt"]
          [:th "Village Name"]
          [:th "Name of the First Party"]
          [:th "Date of Receipt"]
          [:th "Date of Gist Final Order"]
          [:th "Rack NUmber"]
          [:th "Starting Year"]
          [:th "Ending Year"]
          [:th " "]
          [:th " "]
          ]]
        [:tbody
         (for [mt o2registers]
           ^{:key (.-id mt)} [:tr
                              [:td (.-serialnumber mt)]
                              [:td (.-tehsil mt)]
                              [:td (.-dateofinstitution mt)]
                              [:td (.-sourceofreceipt mt)]
                              [:td (.-villagename mt)]
                              [:td (.-nameofthefirstparty mt)]
                              [:td (.-dateofreceiptfrompanchayat mt)]
                              [:td (.-dateandgistoffinalorder mt)]
                              [:td (.-racknumber mt)]
                              [:td (.-startingyear mt)]
                              [:td (.-endingyear mt)]
                              [:td [:a {:href "javascript:;"
                                        :on-click  #(o2register-update(.-id mt))
                                        :class "btn btn-success btn-sm glyphicon glyphicon-edit"}]]
                              [:td  [:a {:href "javascript:;" :on-click #(o2register-delete(.-id mt))
                                         :class "btn btn-danger btn-sm glyphicon glyphicon-remove"}]]])]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])

;; ---------------------------------------------------------
;; o4register-records

(defn o4register-form-validator [data-set]
  (first (b/validate data-set
                     :tehsil [[v/required :message "Field is required"]]
                     :khatakhatuninumber [[v/required :message "Field is required"]]
                     :numberanddateoforder [[v/required :message "Field is required"]]
                     :khasranumber [[v/required :message "Field is required"]]
                     :area [[v/required :message "Field is required"]]
                     :revenuerentofshareofplotstransferred [[v/required :message "Field is required"]]
                     :nameanddescriptionofthepersonsremoved [[v/required :message "Field is required"]]
                     )))

(defn o4register-input-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-element id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (o4register-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((o4register-form-validator @data-set) id)))]])
                        [:div])]])))


(defn o4register-template [doc-name data-set focus save-function]
 [:div.container
  [:div.col-md-12
   [:div.box.box-info
    [:div.box-header.with-border 
     [:h2.box-title doc-name]]
    [:div.form-horizontal
     [:div.box-body
       [o4register-input-row :tehsil "Tehsil" "text" data-set focus]
       [o4register-input-row :khatakhatuninumber "Khata Khatuni Number" "text" data-set focus]
       [o4register-input-row :numberanddateoforder "Number and Date Of Order" "date" data-set focus]
       [o4register-input-row :khasranumber "Khasra Number" "text" data-set focus]
       [o4register-input-row :area "Area" "text" data-set focus]
       [o4register-input-row :revenuerentofshareofplotstransferred "Revenue Rent Of Share Of Plots Transfered" "text" data-set focus]
       [o4register-input-row :nameanddescriptionofthepersonsremoved "Name and Description Of the Persons Removed" "text" data-set focus]
    [:div.box-footer
      [:button.btn.btn-default {:on-click o4register-form-cancel} "Cancel"]
      [:button.btn.btn-info.pull-right {:on-click save-function} "Save"]]]]]]])

(defn o4register-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [o4register-template 
              "o4register Add Form" 
               add-data
               focus
               #(o4register-add-form-onclick add-data focus)])))

(defn o4register-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :tehsil (.-tehsil dmt)
                             :khatakhatuninumber (.-khatakhatuninumber dmt)
                             :numberanddateoforder (.-numberanddateoforder dmt)
                             :khasranumber (.-khasranumber dmt)
                             :area (.-area dmt)
                             :revenuerentofshareofplotstransferred (.-revenuerentofshareofplotstransferred dmt)
                             :nameanddescriptionofthepersonsremoved (.-nameanddescriptionofthepersonsremoved dmt)
                             })
        focus (r/atom nil)]
    (fn [] [o4register-template
            "o4register Update Form"
            update-data 
            focus
            #(o4register-update-form-onclick update-data focus)])))


(defn o4register-add-form-onclick [data-set focus]
  (if (= nil (o4register-form-validator @data-set))
    (let [onres (fn[json] (secretary/dispatch! "/o4register"))]
          (http-post (str serverhost "o4registers") onres  (.serialize (Serializer.) (clj->js @data-set))))
  (reset! focus "on")))


(defn o4register-update-form-onclick [data-set focus]
  (if (= nil (o4register-form-validator @data-set))
      (let [onres (fn[data] (secretary/dispatch! "/o4register"))]
          (http-put (str serverhost "o4registers/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set))))
    (reset! focus "on")))

(defn o4register-form-cancel [event]
  (secretary/dispatch! "/o4register"))

(defn o4register-add [event]
  (secretary/dispatch! "/o4register/add"))

(defn o4register-update[id]
  (secretary/dispatch! (str "/o4register/update/" id)))

(defn o4register-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/o4register"))]
    (http-delete (str serverhost "o4registers/" id)  onres)))


(defroute o4register-list "/o4register" []
  (let [onres (fn [json]
                (let [dt (getdata json)]
                  (set-key-value :o4registers (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-key-value :page-location  [render-o4register (get-value! :o4registers)])))]
    (set-key-value :is-searched-results false)
    (http-get (str serverhost "o4registers?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


(defroute o4register-add-path "/o4register/add" []
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [o4register-add-template])))]
    (http-get (str serverhost "villages") onres)))

(defroute o4register-upd-path "/o4register/update/:id" [id]
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [o4register-update-template id
                                    (first (filter (fn[obj]
                                                     (=(.-id obj) (.parseInt js/window id))) (get-value! :o4registers)))])))]
    (http-get (str serverhost "villages") onres)))



(defn render-o4register [o4registers]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of o4register Records"]]
    [:div.row
     [:div.col-md-12
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click o4register-add}] 
       ;; [:input {:id "getall" :type "button" :value "Refresh"
       ;;          :class "btn btn-primary" :on-click get-all-click}]
       ]
      [:div {:class "box-body"}

       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          [:th "Tehsil"]
          [:th "Khata Khatuni Number"]
          [:th "Number and Date of Order"]
          [:th "Khasra Number"]
          [:th "Area"]
          [:th "Revenue of Share of Plots Transfered"] 
          [:th "Name and Description of the Persons Removed"]         
          [:th " "]
          [:th " "]
          ]]
        [:tbody
         (for [mt o4registers]
           ^{:key (.-id mt)} [:tr
                              [:td (.-tehsil mt)]
                              [:td (.-khatakhatuninumber mt)]
                              [:td (.-numberanddateoforder mt)]
                              [:td (.-khasranumber mt)]
                              [:td (.-area mt)]
                              [:td (.-revenuerentofshareofplotstransferred mt)]
                              [:td (.-nameanddescriptionofthepersonsremoved mt)]
                              [:td [:a {:href "javascript:;"
                                        :on-click  #(o4register-update(.-id mt))
                                        :class "btn btn-success btn-sm glyphicon glyphicon-edit"}]]
                              [:td  [:a {:href "javascript:;" :on-click #(o4register-delete(.-id mt))
                                         :class "btn btn-danger btn-sm glyphicon glyphicon-remove"}]]])]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])


;; ---------------------------------------------------------
;; o6register-records

(defn o6register-form-validator [data-set]
  (first (b/validate data-set
                     :tehsil [[v/required :message "Field is required"]]
                     :year [[v/required :message "Field is required"]]
                     :mehsilnumber [[v/required :message "Field is required"]]
                     :dateoforderlevy [[v/required :message "Field is required"]]
                     :nameofpersonwhomrecoveryismade [[v/required :message "Field is required"]])))


(defn o6register-input-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-element id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (o6register-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((o6register-form-validator @data-set) id)))]])
                        [:div])]])))


(defn o6register-template [doc-name data-set focus save-function]
 [:div.container
  [:div.col-md-12
   [:div.box.box-info
    [:div.box-header.with-border 
     [:h2.box-title doc-name]]
    [:div.form-horizontal
     [:div.box-body 
       [o6register-input-row :tehsil "Tehsil" "text" data-set focus]
       [o6register-input-row :year "Year" "date" data-set focus]
       [o6register-input-row :mehsilnumber "Mehsil Number" "text" data-set focus]
       [o6register-input-row :dateoforderlevy "Date of Order Levy" "date" data-set focus]
       [o6register-input-select "Village Name" data-set focus ]
       [o6register-input-row :nameofpersonwhomrecoveryismade "Name of Person Whom Recovery is Made" "text" data-set focus]
    [:div.box-footer
      [:button.btn.btn-default {:on-click o6register-form-cancel} "Cancel"]
      [:button.btn.btn-info.pull-right {:on-click save-function} "Save"]]]]]]])


(defn o6register-add-form-onclick [data-set focus]
  (if (= nil (o6register-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid (int (.-value (.getElementById js/document "o6register-districts")))))
        (let [onres (fn[json] (secretary/dispatch! "/o6register"))]
          (http-post (str serverhost "o6registers") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on")))


(defn o6register-update-form-onclick [data-set focus]
  (if (= nil (o6register-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :villageid (int (.-value (.getElementById js/document "o6register-districts")))))
        (let [onres (fn[data] (secretary/dispatch! "/o6register"))]
          (http-put (str serverhost "o6registers/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on")))

(defn o6register-form-cancel [event]
  (secretary/dispatch! "/o6register"))

(defn o6register-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get (str serverhost "villages/search?name=" eval) onresp))))


(defn o6register-tags-template [data-set]
  (cond (nil? (:villageid @data-set)) [:select.form-control {:id "o6register-districts"}
                                       (for [d (get-value! :villages)]
                                         ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)])]
        :else [:select.form-control {:id "o6register-districts" :defaultValue (:villageid @data-set)}
               (doall (for [d (get-value! :villages)]
                        ^{:key (.-id d)} [:option {:value (.-id d)} (.-villagename d)]))]))

(defn o6register-input-select [label data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div#tagdiv.col-sm-6 [o6register-tags-template data-set]]
       [:div.col-sm-3 [:div]]])))


(defn o6register-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [o6register-template 
          "o6register Add Form" 
            add-data focus
             #(o6register-add-form-onclick add-data focus)])))

(defn o6register-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :tehsil (.-tehsil dmt)
                             :year (.-year dmt)
                             :mehsilnumber (.-mehsilnumber dmt)
                             :dateoforderlevy (.-dateoforderlevy dmt)
                             :villageid (.-villageid dmt)
                             :villagename (.-villagename dmt)
                             :nameofpersonwhomrecoveryismade (.-nameofpersonwhomrecoveryismade dmt)})
        focus (r/atom nil)]
    (fn [] [o6register-template
            "o6register Update Form"
            update-data focus
            #(o6register-update-form-onclick update-data focus)])))


(defn o6register-update[id]
  (secretary/dispatch! (str "/o6register/update/" id)))

(defn o6register-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/o6register"))]
    (http-delete (str serverhost "o6registers/" id)  onres)))


(defroute o6register-list "/o6register" []
  (let [onres (fn [json]
                (let [dt (getdata json)]
                  (set-key-value :o6registers (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-key-value :page-location  [render-o6register (get-value! :o6registers)])))]
    (set-key-value :is-searched-results false)
    (http-get (str serverhost "o6registers?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


(defroute o6register-add-path "/o6register/add" []
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [o6register-add-template])))]
    (http-get (str serverhost "villages") onres)))

(defroute o6register-upd-path "/o6register/update/:id" [id]
  (let [onres (fn[json](
                        (set-key-value :villages (getdata json))
                        (set-page! [o6register-update-template id
                                    (first (filter (fn[obj]
                                                     (=(.-id obj) (.parseInt js/window id))) (get-value! :o6registers)))])))]
    (http-get (str serverhost "villages") onres)))

(defn o6register-add [event]
  (secretary/dispatch! "/o6register/add"))

(defn render-o6register [o6registers]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of o6register Records"]]
    [:div.row
     [:div.col-md-12
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click o6register-add}] 
       ;; [:input {:id "getall" :type "button" :value "Refresh"
       ;;          :class "btn btn-primary" :on-click get-all-click}]
       ]
      [:div {:class "box-body"}

       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          [:th "Tehsil"]
          [:th "Year"]
          [:th  "Mehsil Number"]
          [:th "Date of Orederlevy"]
          [:th "Village Name"]
          [:th "name of Person Whom Recovery is Made"]
          [:th " "]
          [:th " "]
          ]]
        [:tbody
         (for [mt o6registers]
           ^{:key (.-id mt)} [:tr
                              [:td (.-tehsil mt)]
                              [:td (.-year mt)]
                              [:td (.-mehsilnumber mt)]
                              [:td (.-dateoforderlevy mt)]
                              [:td (.-villagename mt)]
                              [:td (.-nameofpersonwhomrecoveryismade mt)]
                              [:td [:a {:href "javascript:;"
                                        :on-click  #(o6register-update(.-id mt))
                                        :class "btn btn-success btn-sm glyphicon glyphicon-edit"}]]
                              [:td  [:a {:href "javascript:;" :on-click #(o6register-delete(.-id mt))
                                         :class "btn btn-danger btn-sm glyphicon glyphicon-remove"}]]])]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])
;; ----------------------------------------------------------------------------------


(defn table-mount []
  (.ready (js/$ js/document)
          (fn []
            (.DataTable (js/$ "#example1")))))
(defn home [documents]
  (r/create-class {:reagent-render render-mutations
                   :component-did-mount table-mount }))


(defroute home-path "/" []
  (let [onres (fn [json]
                (let [dt (getdata json)]
                  (set-key-value :mutations (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-key-value :page-location  [render-mutations (get-value! :mutations)])))]
    (set-key-value :is-searched-results false)
    (http-get (str serverhost "mutations?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))

(defroute "*" []
  (js/alert "<h1>Not Found Page</h1>"))


(defn main
  []
  (secretary/set-config! :prefix "#")
  (set-key-value :page-location [render-mutations])
  (r/render [page]
            (.getElementById js/document "app1"))
  (let [history (History.)]
    (events/listen history "navigate"
                   (fn [event]
                     (secretary/dispatch! (.-token event))))
    (.setEnabled history true)))

(defn nav! [token]
  (.setToken (History.) token))

(main)
