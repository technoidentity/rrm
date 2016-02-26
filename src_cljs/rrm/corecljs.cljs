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
                          :districts nil
                          :subdivisions nil
                          :villages {}
                          :o2mutations {}
                          :o4mutations {}
                          :o6mutations {}
                          :token ""
                          :is-searched-results false
                          :user nil
                          :mutationnumbers nil}))

(def serverhost "http://localhost:9000/")

(defn set-key-value [k v]
  (reset! storage (assoc @storage k v)))

(defn http-get [url callback]
  (xhr/send url callback))

(def button-tool-bar (r/adapt-react-class (aget js/ReactBootstrap "ButtonToolbar")))
(def button (r/adapt-react-class (aget js/ReactBootstrap "Button")))


(defn get-value! [k]
  (k @storage))

(defn http-get-auth [url callback]
  (xhr/send url callback "GET" "" (clj->js {:rrm-auth (get-value! :token)})))

(defn page []
  (get-value! :page-location))

(defn getinputvalue[id]
  (.-value (.getElementById js/document id)))

(defn getdata [res]
  (.getResponseJson (.-target res)))

(defn get-status [res]
  (.getStatus (.-target res)))

(defn http-post [url callback data]
  (xhr/send url callback "POST" data  (structs/Map. (clj->js {:Content-Type "application/json" :rrm-auth (get-value! :token)}))))

(defn http-put [url callback data]
  (xhr/send url callback "PUT" data  (structs/Map. (clj->js {:Content-Type "application/json" :rrm-auth (get-value! :token)}))))

(defn http-delete [url callback]
  (xhr/send url callback "DELETE" "" (structs/Map. (clj->js {:Content-Type "application/json" :rrm-auth (get-value! :token)}))))


;; =====================================================================================================
;; login-form with validations
;; =====================================================================================================

(defn set-login-page []
  (do
    (set! (.-className (dom/getElement "body")) "hold-transistion skin-blue")
    (set! (.-className (dom/getElement "wrapper")) "")
    (set! (.-display (.-style (dom/getElement "mainsb"))) "none")
    (set! (.-className (dom/getElement "cntwrapper")) "")
    (set! (.-display (.-style (dom/getElement "footer"))) "none")
    (set! (.-display (.-style (dom/getElement "un2"))) "none")))

(defn reset-login-page []
  (let [cusr (.-username (get-value! :user))]
    (set! (.-className (dom/getElement "body")) "skin-blue sidebar-mini")
    (set! (.-className (dom/getElement "wrapper")) "wrapper")
    (set! (.-display (.-style (dom/getElement "mainsb"))) "block")
    (set! (.-className (dom/getElement "cntwrapper")) "content-wrapper")
    (set! (.-display (.-style (dom/getElement "footer"))) "block")
    (set! (.-display (.-style (dom/getElement "un2"))) "block")
    (set! (.-innerHTML (dom/getElement "un")) cusr)
    (set! (.-innerHTML (dom/getElement "un1")) cusr)))


(defn login-validator [data-set]
  (first (b/validate data-set
                     :username [[v/required :message "Filed is required"]
                                [v/email :message "Enter valid email-id"]]
                     :password [[v/required :message "Filed is required"]
                                [v/string  :message "Enter valid password"]])))

(defn input-element [id ttype data-set placeholder in-focus]
  [:input.form-control {:id id
                        :type ttype
                        :value (@data-set id)
                        :placeholder placeholder
                        :on-change #(swap! data-set assoc id (-> % .-target .-value))
                        :on-blur  #(reset! in-focus "on")
                        }])

(defn login-input [id span-class ttype data-set label focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group.has-feedback
        [input-element id ttype data-set label input-focus]
        [:span {:class span-class}]
        (if (or @input-focus @focus)
          (if (= nil (login-validator @data-set))
            [:div]
            [:div {:style  {:color "red"}} [:b (str (first ((login-validator @data-set) id)))]])
          [:div])])))

(defn submit-login [data-set focus]
  (if (= nil (login-validator @data-set))
    (let [onresp (fn [json]
                   (if (= (get-status json) 200)
                     ((set-key-value :user (.-_2 (getdata json)))
                      (set-key-value :token (.-_1 (getdata json)))
                      ;;(js/console.log (.-_2 (getdata json)))
                      (reset-login-page)
                      (secretary/dispatch! "/"))))]
      (http-post (str serverhost "login") onresp (.serialize (Serializer.) (clj->js @data-set ))))
    (reset! focus "on")))

(defn submit-button [data-set focus]
  [:div.row
   [:div.col-md-4
    [:button {:class "btn btn-primary btn-block btn-flat" :on-click #(submit-login data-set focus)} "Sign In" ]]])

(defn login []
  (let [my-data (r/atom  {})
        focus (r/atom nil)]
    (fn []
      [:div.hold-transition.login-page {:style {:width "100%" :height "100%"}}
       [:div.login-box
        [:div.login-logo
         [:b "Log-in"]]
        [:div.login-box-body
         [:p.login-box-msg "Sign in to start your session"]
         [login-input :username "glyphicon glyphicon-envelope form-control-feedback" "email" my-data "Email" focus]
         [login-input :password "glyphicon glyphicon-lock form-control-feedback" "password" my-data "password" focus]
         [submit-button my-data focus ]]]])))


;; ====================================================================================================
;; end of login-form
;; ====================================================================================================

(defn is-authenticated? []
  (not (nil? (get-value! :user))))

(defn is-admin-or-super-admin []
  (let [role (.-role (get-value! :user))]
    (or (= role "admin")
        (= role "superadmin"))))

(defn set-page! [currnt-page]
  (if (nil? (get-value! :user)) (set-key-value :page-location [login])
      (set-key-value :page-location
                     currnt-page)))
(defn sign-out []
  (set-key-value :user nil)
  (set-page! [login])
  (set-login-page))

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


(declare render-mutations)

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
                  (set-page! [render-mutations (get-value! :mutations)])))]
    (http-get-auth (get-index-url (get-value! :is-searched-results)
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
                          (set-key-value :mutations (.-data data))
                          (set-key-value :total-pages
                                         (get-total-rec-no (.-pagesCount data)))
                          (set-page! [render-mutations (get-value! :mutations)])))]
    (set-key-value :current-page 1)
    (set-key-value :is-searched-results true)
    (http-get-auth (str (get-search-url
                         vid mn fp sp po
                         kknum so2 st knum so4 so6)"&pageIndex=0&pageSize=10") onres)))

;; (defn input [label type id]
;;   (row label [:input.form-control {:type type :id id :on-change on-change}]))

;; --------------------------------------------------------------------------
;; mutation form

(defn form-validator [data-set]
  (first (b/validate data-set
                     :mutationnumber [[v/required :message "Field is required"]]
                     :nameofthefirstparty [[v/required :message "Field is required"]]
                     :nameofthesecondparty [[v/required :message "Field is required"]]
                     :dateofinstitution [[v/required :message "Field is required"]]
                     :nameofpo [[v/required :message "Field is required"]]
                     :dateofdecision [[v/required :message "Field is required"]
                                      [v/datetime :message "Please enter valid date"]]
                     :title [[v/required :message "Field is required"]]
                     :khasranumber [[v/required :message "Field is required"]]
                     ;; :area [[v/required :message "Field is required"]]
                     :khatakhatuninumber [[v/required :message "Field is required"]]
                     ;; :o2number [[v/required :message "Field is required"]]
                     ;; :o4number [[v/required :message "Field is required"]]
                     ;; :o6number [[v/required :message "Field is required"]]
                     :racknumber [[v/required :message "Field is required"]]
                     ;; :receiveddate [[v/required :message "Field is required"]]
                     ;; :remarks [[v/required :message "Field is required"]]
                     )))

(defn mutation-input-element [id ttype data-set placeholder in-focus bool]
  [:input.form-control {:id id
                        :type ttype
                        :value (@data-set id)
                        :placeholder placeholder
                        :on-change #(swap! data-set assoc id (-> % .-target .-value))
                        ;; :on-blur  #(reset! in-focus "on")
                        :disabled bool
                        }])

(defn form-input-element [id label ttype data-set focus bool]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [mutation-input-element id ttype data-set label input-focus bool]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((form-validator @data-set) id)))]])
                        [:div])]])))

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
                        ;; :on-blur  #(reset! in-focus "on")
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

(defn dist-onchange [id val data-set]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (set-key-value :subdivisions dt)
                (set-key-value :villages nil)))]
    (do (http-get-auth (str  serverhost "districts/" val  "/subdivisions") res)
        (swap! data-set assoc id val))))

(defn dist-sel-tag [id data data-set]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(dist-onchange id (-> % .-target .-value) data-set) }
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d)}
     [:option {:value (.-id d)} (.-name d)])])

(defn form-dist-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [dist-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])

(defn sub-onchange [id val data-set]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (set-key-value :villages dt)))]
    (do (http-get-auth (str serverhost "subdivisions/" val  "/villages") res)
        (swap! data-set assoc id val))))

(defn sub-sel-tag [id data data-set]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(sub-onchange  id (-> % .-target .-value) data-set)}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-subdivisionname d)])])

(defn form-sub-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [sub-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])


(defn villages-sel-tag [id data data-set ]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(swap! data-set assoc id (js/parseInt (-> % .-target .-value)))}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-villagename d)])])

(defn form-villages-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [villages-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])


(defn form-cancel [event]
  (secretary/dispatch! "/"))



(defn date-input [id data-set placeholder bool]
  [:input.form-control {:id id
                        :type "date"
                        :value (@data-set id)
                        :placeholder placeholder
                        :on-change #(swap! data-set assoc id (-> % .-target .-value))
                        :disabled bool
                        }])

(defn add-check-input-element [id label data-set focus bool]
  (let [check (r/atom true)]
    (fn []
      [:div
       [:div.form-group
        [:div.col-sm-3
         [:input {:type "checkbox"
                  :style {:width "17px" :height "17px" :float "right"}
                  :on-change #(swap! check not)}]] 
        [:label.col-sm-6 "Out side Record Room"]
        [:div.col-sm-3 [:div]]]
       (if @check
         [:div]
         [:div
          [:div.form-group
           [:label.col-sm-3.control-label label]
           [:div.col-sm-6 [date-input id data-set label false]]
           [:div.col-sm-3 [:div]]]
          [form-input-element :remarks "Remarks" "text" data-set focus bool]
          ])])))

(defn reset-mut-combo-boxes []
  (set! (.-value (dom/getElement "districtid")) 0)
  (set-key-value :subdivisions [])
  (set-key-value :villages []))


(defn add-mutation-template [doc-name data-set focus save-function bool]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [form-input-element :mutationnumber "Mutation Number" "text" data-set focus false]
       [form-input-element :nameofthefirstparty "Name of the First Party" "text" data-set focus false]
       [form-input-element :nameofthesecondparty "Name of the Second Party" "text" data-set focus false]
       [form-input-element :dateofinstitution "Date of Institution" "date" data-set focus false]
       [form-input-combo   :nameofpo "Name of PO" data-set focus ]
       [form-input-element :dateofdecision "Date of Decision" "date" data-set focus false]
       [form-input-element :title "Title" "text" data-set focus false]
       [form-input-element :khasranumber "Khasra Number" "text" data-set focus false]
       [form-input-element :area "Area" "text" data-set focus false]
       [form-input-element :khatakhatuninumber "Khata Khatuni Number" "text" data-set focus false]
       [form-dist-sel "District" :districtid (:districts @storage) data-set]
       [form-sub-sel "Sub-division Name" :subdivisionid (:subdivisions @storage) data-set]
       [form-villages-sel "Village Name" :villageid (:villages @storage) data-set]
       [form-input-element :o2number "O2 Number" "text" data-set focus false]
       [form-input-element :o4number "O4 Number" "text" data-set focus false ]
       [form-input-element :o6number "O6 Number" "text" data-set focus false]
       [form-input-element :racknumber "Rack Number" "text" data-set focus false]
       [add-check-input-element :senddate "Send Date" data-set focus bool]
       ]
      [:div.box-footer
       [:div.col-sm-12.col-md-offset-5
        [button-tool-bar
         [button {:bs-style "success" :on-click save-function} "Save"]
         (when (nil? (:id @data-set)) [button {:bs-style "info" :on-click #((reset! data-set {:isactive true})
                                                                            (reset-mut-combo-boxes))} "Refresh"])
         [button {:bs-style "danger" :on-click form-cancel } "Cancel"]]]
       ;;[:span (str @data-set)]
       ]]]]])

(defn upd-check-input-element [data-set focus bool ]
  (let [check (r/atom true)]
    (fn []
      [:div
       [:div.form-group
        [:label.col-sm-3.control-label "Send Date"]
        [:div.col-sm-6 [date-input :senddate data-set "Send Date" true]]
        [:div.col-sm-3 [:div]]]
       (if (:receiveddate @data-set)
         [:div
          [:div.form-group
           [:label.col-sm-3.control-label "Received Date"]
           [:div.col-sm-6 [date-input :receiveddate data-set "Received Date" false]]
           [:div.col-sm-3 [:div]]]
          [form-input-element :remarks "Remarks" "text" data-set focus bool]]
         [:div
          [:div.form-group
           [:div.col-sm-3
            [:input {:type "checkbox"
                     :style {:width "17px" :height "17px" :float "right"}
                     :on-change #(swap! check not)}]] 
           [:label.col-sm-6 "Check here to enter received date"]
           [:div.col-sm-3 [:div]]]
          (if @check
            [:div]
            [:div
             [:div.form-group
              [:label.col-sm-3.control-label "Received Date"]
              [:div.col-sm-6 [date-input :receiveddate data-set "Received Date" false]]
              [:div.col-sm-3 [:div]]]])
          [form-input-element :remarks "Remarks" "text" data-set focus bool]])])))



(defn update-mutation-template [doc-name data-set focus save-function bool]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [form-input-element :mutationnumber "Mutation Number" "text" data-set focus false]
       [form-input-element :nameofthefirstparty "Name of the First Party" "text" data-set focus false ]
       [form-input-element :nameofthesecondparty "Name of the Second Party" "text" data-set focus false]
       [form-input-element :dateofinstitution "Date of Institution" "date" data-set focus false]
       [form-input-combo   :nameofpo "Name of PO" data-set focus ]
       [form-input-element :dateofdecision "Date of Decision" "date" data-set focus false]
       [form-input-element :title "Title" "text" data-set focus false]
       [form-input-element :khasranumber "Khasra Number" "text" data-set focus false]
       [form-input-element :area "Area" "text" data-set focus false]
       [form-input-element :khatakhatuninumber "Khata Khatuni Number" "text" data-set focus false]
       [form-dist-sel "District" :districtid (:districts @storage) data-set]
       [form-sub-sel "Sub-division Name" :subdivisionid (:subdivisions @storage) data-set]
       [form-villages-sel "Village Name" :villageid (:villages @storage) data-set]
       [form-input-element :o2number "O2 Number" "text" data-set focus false]
       [form-input-element :o4number "O4 Number" "text" data-set focus false]
       [form-input-element :o6number "O6 Number" "text" data-set focus false]
       [form-input-element :racknumber "Rack Number" "text" data-set focus false]
       [upd-check-input-element data-set focus bool]
       ]
      [:div.box-footer
       [:div.col-sm-12.col-md-offset-5 
        [button-tool-bar
         [button {:bs-style "success" :on-click save-function } "Save"]
         [button {:bs-style "danger" :on-click form-cancel } "Cancel"]]
       ;;[:span (str @data-set)]
       ]]]]]])


(defn map-mutation-data [mut]
  {:id (if (nil? (mut :id))0 (int (mut :id))) :nameofthefirstparty (mut :nameofthefirstparty)
   :nameofthesecondparty (:nameofthesecondparty mut) :dateofinstitution (mut :dateofinstitution) :nameofpo (mut :nameofpo)
   :dateofdecision (:dateofdecision mut) :title (:title mut) :area (:area mut)
   :numbers {:mutationnumber (mut :mutationnumber) :khasranumber (mut :khasranumber) :racknumber (mut :racknumber)
             :o4number (mut :o4number) :o2number (mut :o2number) :khatakhatuninumber (mut :khatakhatuninumber)
             :o6number (mut :o6number)}
   :village {:id (str (mut :villageid))  :name ""}
   :subdivision {:id (mut :subdivisionid) :name ""}
   :district {:id (mut :districtid) :name ""}
   :senddate (mut :senddate)
   :receiveddate (mut :receiveddate)
   :remarks (mut :remarks)})


(defn add-form-onclick [data-set focus]
  (if (= nil (form-validator @data-set))
    (let [onres (fn[json]
                  (reset! data-set {:isactive true}))]
      (http-post (str serverhost "mutations")
                 onres  (.serialize (Serializer.) (clj->js (map-mutation-data @data-set)))))
    (reset! focus "on")))

(defn update-form-onclick [data-set focus]
  (if (= nil (form-validator @data-set))
    (let [onres (fn[data]
                  (secretary/dispatch! "/"))]
      (when (and (not (nil? (:senddate @data-set)))
                 (nil? (:receiveddate @data-set))) (swap! data-set assoc :racknumber ""))
      (http-put (str serverhost "mutations/" (:id @data-set))
                onres (.serialize (Serializer.) (clj->js (map-mutation-data @data-set))))))
  (reset! focus "on"))


(defn mutation-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [add-mutation-template "Mutation Add Form" add-data focus #(add-form-onclick add-data focus) false])))


(defn mutation-update-template [id dmt]
  (do
    (if (and (not= nil (.-senddate dmt)) (not= nil (.-receiveddate dmt)))
      (let [update-data (r/atom {:id (int id)
                                 :mutationnumber (.-mutationnumber (.-numbers dmt))
                                 :nameofthefirstparty (.-nameofthefirstparty dmt)
                                 :nameofthesecondparty (.-nameofthesecondparty dmt)
                                 :dateofinstitution (.-dateofinstitution dmt)
                                 :nameofpo (.-nameofpo dmt)
                                 :dateofdecision (.-dateofdecision dmt)
                                 :title (.-title dmt)
                                 :khasranumber (.-khasranumber (.-numbers dmt))
                                 :khatakhatuninumber (.-khasranumber (.-numbers dmt))
                                 :area (.-area dmt)
                                 :o2number (.-o2number (.-numbers dmt))
                                 :o4number (.-o4number (.-numbers dmt))
                                 :o6number (.-o6number (.-numbers dmt))
                                 :racknumber (.-racknumber (.-numbers dmt))
                                 :senddate nil
                                 :receiveddate nil
                                 :remarks nil
                                 :villageid (.-id (.-village dmt))
                                 :subdivisionid (.-id (.-subdivision dmt))
                                 :districtid (.-id (.-district dmt))
                                 :isactive true})
            focus (r/atom nil)]
        (fn []
          [add-mutation-template
           "Mutation Update Form"
           update-data
           focus
           #(update-form-onclick update-data focus) false]))
      (if (= nil (.-senddate dmt))
        (let [update-data (r/atom {:id (int id)
                                   :mutationnumber (.-mutationnumber (.-numbers dmt))
                                   :nameofthefirstparty (.-nameofthefirstparty dmt)
                                   :nameofthesecondparty (.-nameofthesecondparty dmt)
                                   :dateofinstitution (.-dateofinstitution dmt)
                                   :nameofpo (.-nameofpo dmt)
                                   :dateofdecision (.-dateofdecision dmt)
                                   :title (.-title dmt)
                                   :khasranumber (.-khasranumber (.-numbers dmt))
                                   :khatakhatuninumber (.-khasranumber (.-numbers dmt))
                                   :area (.-area dmt)
                                   :o2number (.-o2number (.-numbers dmt))
                                   :o4number (.-o4number (.-numbers dmt))
                                   :o6number (.-o6number (.-numbers dmt))
                                   :racknumber (.-racknumber (.-numbers dmt))
                                   :senddate (.-senddate dmt)
                                   :receiveddate (.-receiveddate dmt)
                                   :remarks (.-remarks dmt)
                                   :villageid (.-id (.-village dmt))
                                   :subdivisionid (.-id (.-subdivision dmt))
                                   :districtid (.-id (.-district dmt))
                                   :isactive true})
              focus (r/atom nil)]
          (fn []
            [add-mutation-template
             "Mutation Update Form"
             update-data
             focus
             #(update-form-onclick update-data focus) false]))
        (let [update-data (r/atom {:id (int id)
                                   :mutationnumber (.-mutationnumber (.-numbers dmt))
                                   :nameofthefirstparty (.-nameofthefirstparty dmt)
                                   :nameofthesecondparty (.-nameofthesecondparty dmt)
                                   :dateofinstitution (.-dateofinstitution dmt)
                                   :nameofpo (.-nameofpo dmt)
                                   :dateofdecision (.-dateofdecision dmt)
                                   :title (.-title dmt)
                                   :khasranumber (.-khasranumber (.-numbers dmt))
                                   :khatakhatuninumber (.-khasranumber (.-numbers dmt))
                                   :area (.-area dmt)
                                   :o2number (.-o2number (.-numbers dmt))
                                   :o4number (.-o4number (.-numbers dmt))
                                   :o6number (.-o6number (.-numbers dmt))
                                   :racknumber (.-racknumber (.-numbers dmt))
                                   :senddate (.-senddate dmt)
                                   :receiveddate (.-receiveddate dmt)
                                   :remarks (.-remarks dmt)
                                   :villageid (.-id (.-village dmt))
                                   :subdivisionid (.-id (.-subdivision dmt))
                                   :districtid (.-id (.-district dmt))
                                   :isactive true})
              focus (r/atom nil)]
          (fn []
            [update-mutation-template
             "Mutation Update Form"
             update-data
             focus
             #(update-form-onclick update-data focus) true]))))))
  
(defn click-update[id]
  (secretary/dispatch! (str "/mutations/update/" id)))

(defn delete[id]
  (let [del-conf? (js/confirm "Are you sure you want to delete?")
        onres (fn [json]
                (secretary/dispatch! "/"))]
    (when del-conf? (http-delete (str serverhost "mutations/" id)  onres))))

(defn add [event]
  (secretary/dispatch! "/mutations/add"))

(defn get-all-click [event]
  (let [onres (fn [json]
                (let [mt (getdata json)]
                  (set-key-value :mutations (.-data mt))
                  (set-key-value :total-pages (get-total-rec-no
                                               (.-pagesCount mt)))
                  (set-key-value :current-page 1)
                  (set-page! [render-mutations (get-value! :mutations)])))]
    (set-key-value :is-searched-results false)
    (http-get-auth (str serverhost "mutations?pageIndex=0&pageSize=10") onres)))


(defn src-dist-onchange [val]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (set-key-value :subdivisions dt)
                (set-key-value :villages nil)))]
    (http-get-auth (str  serverhost "districts/" val  "/subdivisions") res)))

(defn src-dist-sel-tag []
  [:select.form-control {:id :src-dist
                         :placeholder "District Name"
                         :on-change #(src-dist-onchange (-> % .-target .-value)) }
   [:option {:value 0} "--Select--"]
   (for [d  (@storage :districts)]
     ^{:key (.-id d)}
     [:option {:value (.-id d)} (.-name d)])])

(defn src-sub-onchange [ val]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (set-key-value :villages dt)))]
    (http-get-auth (str serverhost "subdivisions/" val  "/villages") res)))

(defn src-sub-sel-tag []
  [:select.form-control {:id :src-sub
                         :placeholder "Sub Division Name"
                         :on-change  #(src-sub-onchange (-> % .-target .-value)) }
   [:option {:value 0} "--Select--"]
   (for [d  (@storage :subdivisions)]
     ^{:key (.-id d)}
     [:option {:value (.-id d)} (.-subdivisionname d)])])

(defn src-vill-sel-tag []
  [:select.form-control {:id :src-vill
                         :placeholder "Village Name"
                          }
   [:option {:value 0} "--Select--"]
   (for [d  (@storage :villages)]
     ^{:key (.-id d)}
     [:option {:value (.-id d)} (.-villagename d)])])


(defn get-data [val]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (swap! storage assoc :mutationnumbers dt)))]
    (http-get (str  serverhost  "mutations/pluck?column=mutationnumber&value=" val ) res)))

(defn datalist1 [data]
  [:datalist {:id "combo1"}
   (for [i data]
     ^{:key i}
     [:option {:value i}])])

(defn render-mutations [mutations]
  [:div.col-md-12
   [:div {:class "box"}
    [:div {:class "box-header"}
      [:h3.box-title "Search for Mutation Records"]]
    [:div.box-body
     [:div.form-group
      [:div.row
       [:div.col-sm-2
        [:label "Mutation Number"]
        [:input.form-control {:id "mutationnumber"
                              :list "combo1"
                              :type "text"
                              :placeholder  "Enter search by Mutation Number"
                              :on-change #(get-data (-> % .-target .-value)) 
                              }]
        [datalist1 (:mutationnumbers @storage)]]]]

     [:div.form-group
      [:div.row
       [:span {:style {:float "right" :width "50%"}} [:b "OR"]]
       [:hr]]]

     [:div.form-group
      [:div.row
       [:div.col-sm-2
        [:label "District Name"]
        [src-dist-sel-tag]]
       [:div.col-sm-2
        [:label"Sub Division Name"]
        [src-sub-sel-tag]]
       [:div.col-sm-2
        [:label "Village Name"]
        [src-vill-sel-tag]]
       [:div.col-sm-2
        [:label "O2 Number"]
        [:input.form-control {:id "so2number"
                              :type "text"
                              :placeholder "O2 Number"}]]
       [:div.col-sm-2
        [:label "O4 Number"]
        [:input.form-control {:id "so4number"
                              :type "text"
                              :placeholder "O4 Number"}]]
       [:div.col-sm-2
        [:label "O6 Number"]
        [:input.form-control {:id "so6number"
                              :type "text"
                              :placeholder "O6 Number"}]]]]
     [:div.form-group
      [:div.row
       [:div.col-sm-2
        [:label "Name of the First Party"]
        [:input.form-control {:id "snameofthefirstparty"
                              :type "text"
                              :placeholder "Name of the First Party"}]]
       [:div.col-sm-2
        [:label "Name of the Second Party"]
        [:input.form-control {:id "snameofthesecondparty"
                              :type "text"
                              :placeholder "Name of the Second Party"}]]
       [:div.col-sm-2
        [:label "Name of P.O"]
        [:input.form-control {:id "svillagename"
                              :list "combo"
                              :placeholder "Name of P.O"}[datalist]]]
       [:div.col-sm-2
        [:label "Title"]
        [:input.form-control {:id "stitle"
                              :type "text"
                              :placeholder "Title"}]]
       [:div.col-sm-2
        [:label "Khasra Number"]
        [:input.form-control {:id "skhasranumber"
                              :type "text"
                              :placeholder "Khasra Number"} ]]
       [:div.col-sm-2
        [:label "Khata khatuni Number"]
        [:input.form-control {:id "skhatakhatuninumber"
                              :type "text"
                              :placeholder "Khata Khatuni Number"}]]]]
     [:hr]
     [:div.form-group
      [:div.row
       [:div.col-sm-8.col-md-offset-5 
        [button-tool-bar
         [button {:bs-style "primary" :on-click search } "Search"]
         [button {:bs-style "primary"  :on-click add} "Add New"]
         [button {:id "getall" :bs-style "primary" :on-click get-all-click} "Refresh"]]
        ]]]]]

   [:div.box
    [:div.box-header
     [:h3.box-title "List of Mutations"]]
    [:div {:class "box-body table-responsive"}
     [:table {:class "table table-bordered table-striped"}
      [:thead
       [:tr 
        (when (is-admin-or-super-admin) [:th ""])
        (when (is-admin-or-super-admin) [:th ""])
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
        [:th "Khasra Number"]
        [:th "Khata khatuni Number"]
        [:th "Rack Number"]
        [:th "Sent Date"]
        [:th "Received Date "]
        ]]
      [:tbody
       (doall (for [mt mutations]
                ^{:key (.-id mt)} [:tr
                                   (when (and  (.-senddate mt) (not (.-receiveddate mt)))
                                     {:style {:background-color "#fbcfd1"}})
                                   (when (is-admin-or-super-admin) [:td  [button {:bs-style "success"  :on-click  #(click-update (.-id mt))} "Update"]])
                                   (when (is-admin-or-super-admin) [:td  [button {:bs-style "danger"  :on-click  #(delete (.-id mt))} "Delete"]])
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
                                   [:td (.-khasranumber (.-numbers mt))]
                                   [:td (.-khatakhatuninumber (.-numbers mt))]
                                   [:td (.-racknumber (.-numbers mt))]
                                   [:td (.-senddate mt)]
                                   [:td (.-receiveddate mt)]
                                   ]))]]]
    [:div{:class "col-xs-6 col-centered col-max"}] [shared-state 0]]])


(defroute mutations-list "/mutations" []
  (let [onres (fn [json]
                (let [dt (getdata json)]
                  (set-key-value :mutations (.-data dt))
                  (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                  (set-page!  [render-mutations (get-value! :mutations)])))]
    (set-key-value :is-searched-results false)
    (http-get-auth (str serverhost "mutations?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


(defroute documents-path "/mutations/add" []
  (let [onres (fn[json](
                       (set-key-value :districts (getdata json))
                       (set-page! [mutation-add-template])))]
    (http-get-auth (str serverhost "districts") onres)))

(defroute documents-path1 "/mutations/update/:id" [id]
  (let [upd-data (first (filter (fn[obj] (=(.-id obj) (.parseInt js/window id))) (get-value! :mutations)))
        vill-res (fn[json](set-key-value :villages (getdata json)))
        sub-res (fn[json](set-key-value :subdivisions (getdata json)))
        dist-res (fn[json] ((set-key-value :districts (getdata json))
                           (set-page! [mutation-update-template id upd-data])))]
    (do
      (http-get-auth (str serverhost "districts") dist-res)
      (http-get-auth (str serverhost "villages") vill-res)
      (http-get-auth (str serverhost "subdivisions") sub-res))))



;; ---------------------------------------------------------
;; rvenue-records

(defn revenue-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber [[v/required :message "Field is required Must be number"]]
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


(defn rev-sub-onchange [id val data-set]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (set-key-value :villages dt)))]
    (do (http-get-auth (str serverhost "subdivisions/" val  "/villages") res)
        (swap! data-set assoc id (js/parseInt val)))))

(defn rev-sub-sel-tag [id data data-set]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(rev-sub-onchange  id (-> % .-target .-value) data-set)}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-subdivisionname d)])])

(defn rev-form-sub-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [rev-sub-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])


(defn rev-villages-sel-tag [id data data-set ]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(swap! data-set assoc id (js/parseInt (-> % .-target .-value)))}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-villagename d)])])

(defn rev-form-villages-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [rev-villages-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])

(defn revenue-form-cancel [event]
  (secretary/dispatch! "/revenue"))

(defn revenue-template [doc-name data-set focus save-function]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [revenue-input-int-row :serialnumber "Serial Number" "text" data-set focus]
       [rev-form-sub-sel "Sub Division Name" :subdivisionid (:subdivisions @storage) data-set]
       [rev-form-villages-sel "Village Name" :villageid (:villages @storage) data-set]
       ;; [revenue-input-row :subdivisionname "Sub Division Name" "text" data-set focus]
       ;; [revenue-input-select "Village Name" data-set focus ]
       [revenue-input-row :year "Year" "date" data-set focus]
       [revenue-input-row :racknumber "Rack Number" "text" data-set focus]
       [revenue-input-row :description "Description" "text" data-set focus]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5 
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click revenue-form-cancel } "Cancel"]]]
        ]]]]]])


(defn revenue-add-form-onclick [data-set focus]
  (if (= nil (revenue-form-validator @data-set))
    (let [onres (fn[json] (secretary/dispatch! "/revenue"))]
          (http-post (str serverhost "revenuerecords") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on"))


(defn revenue-update-form-onclick [data-set focus]
  (if (= nil (revenue-form-validator @data-set))
    (let [onres (fn[data] (secretary/dispatch! "/revenue"))]
          (http-put (str serverhost "revenuerecords/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on"))



(defn revenue-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get-auth (str serverhost "villages/search?name=" eval) onresp))))


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
                             :subdivisionid (.-subdivisionid dmt)
                             :villageid (.-villageid dmt)
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


(defroute revenue-add-path "/revenue/add" []
  (let [onres (fn[json](
                       (set-key-value :subdivisions (getdata json))
                       (set-page! [revenue-add-template])))]
    (http-get-auth (str serverhost "subdivisions") onres)))

(defroute revenue-upd-path "/revenue/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [revenue-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :revenues)))])))
        sub-res (fn[json](set-key-value :subdivisions (getdata json)))]
    (do
      (http-get-auth (str serverhost "villages") onres)
      (http-get-auth (str serverhost "subdivisions") sub-res))))

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
                :class "btn btn-primary" :on-click revenue-add}]]
      [:div {:class "box-body"}

       [:table {:class "table table-bordered table-striped dataTable" :border "2" :width "100px" :style {:table-layout "fixed"}}
        [:thead
         [:tr
          (when (is-admin-or-super-admin)[:th " "])
          (when (is-admin-or-super-admin)[:th " "])
          [:th "S.No"]
          [:th "Sub Division Name"]
          [:th "Name of the Village"]
          ;; [:th "Tehsil"]
          [:th "Year"]
          [:th "Rack Number"]
          [:th "Description"]]]
        [:tbody
         (doall (for [mt revenues]
                  ^{:key (.-id mt)} [:tr

                                     (when (is-admin-or-super-admin)[:td [button {:bs-style "success"
                                                                                  :on-click  #(revenue-update(.-id mt))} "Update"]])
                                     (when (is-admin-or-super-admin)[:td  [button {:bs-style "danger" :on-click #(revenue-delete(.-id mt))} "Delete"]])
                                     [:td (.-serialnumber mt)]
                                     [:td (.-subdivisionname mt)]
                                     [:td (.-villagename mt)]
                                     ;; [:td (.-tehsil mt)]
                                     [:td (.-year mt)]
                                     [:td (.-racknumber mt)]
                                     [:td (.-description mt)]]))]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])


(defroute revenue-list "/revenue" []
  (if (nil? (get-value! :user)) (set-page! [login])
      (let [onres (fn [json]
                    (let [dt (getdata json)]
                      (set-key-value :revenues (.-data dt))
                      (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                      (set-page!  [render-revenue (get-value! :revenues)])))]
        (set-key-value :is-searched-results false)
        (http-get-auth (str serverhost "revenuerecords?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres))))


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

(defn khasragirdwani-form-cancel [event]
  (secretary/dispatch! "/khasragirdwani"))

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
       [khasragirdwani-input-row :description "Description" "text" data-set focus]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5 
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click khasragirdwani-form-cancel } "Cancel"]]]
        ]]]]]])


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



(defn khasragirdwani-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get-auth (str serverhost "villages/search?name=" eval) onresp))))


(defn khasragirdwani-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [khasragirdwani-template
           "Khasragirdwani Add Form"
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
            "Khasragirdwani Update Form"
            update-data focus
            #(khasragirdwani-update-form-onclick update-data focus)])))


(defn khasragirdwani-update[id]
  (secretary/dispatch! (str "/khasragirdwani/update/" id)))

(defn khasragirdwani-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/khasragirdwani"))]
    (http-delete (str serverhost "khasragirdwanis/" id)  onres)))


(defroute khasragirdwani-add-path "/khasragirdwani/add" []
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [khasragirdwani-add-template])))]
    (http-get-auth (str serverhost "villages") onres)))

(defroute khasragirdwani-upd-path "/khasragirdwani/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [khasragirdwani-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id)))
                                                  (get-value! :khasragirdwanis)))])))]
    (http-get-auth (str serverhost "villages") onres)))

(defn khasragirdwani-add [event]
  (secretary/dispatch! "/khasragirdwani/add"))

(defn render-khasragirdwani [khasragirdwanis]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of Khasragirdwani Records"]]
    [:div.row
     [:div.col-sm-12
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
          (when (is-admin-or-super-admin)[:th " "])
          (when (is-admin-or-super-admin)[:th " "])
          [:th "S.No"]
          [:th "Name of the Village"]
          [:th "Sub Division Name"]
          [:th "Tehsil"]
          [:th "Year"]
          [:th "Rack Number"]
          [:th "Description"]
          ]]
        [:tbody
         (doall (for [mt khasragirdwanis]
                  ^{:key (.-id mt)} [:tr

                                     (when (is-admin-or-super-admin)[:td [button {:bs-style "success"
                                                                                  :on-click  #(khasragirdwani-update(.-id mt))} "Update"]])
                                     (when (is-admin-or-super-admin)[:td  [button {:bs-style "danger" :on-click #(khasragirdwani-delete(.-id mt))} "Delete"]])
                                     [:td (.-serialnumber mt)]
                                     [:td (.-villagename mt)]
                                     [:td (.-subdivisionname mt)]
                                     [:td (.-tehsil mt)]
                                     [:td (.-year mt)]
                                     [:td (.-racknumber mt)]
                                     [:td (.-description mt)]]))]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])



(defroute khasragirdwani-list "/khasragirdwani" []
  (if (nil? (get-value! :user)) (set-page! [login])
      (let [onres (fn [json]
                    (let [dt (getdata json)]
                      (set-key-value :khasragirdwanis (.-data dt))
                      (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                      (set-page!  [render-khasragirdwani (get-value! :khasragirdwanis)])))]
        (set-key-value :is-searched-results false)
        (http-get-auth (str serverhost "khasragirdwanis?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres))))


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


(defn masavi-form-cancel [event]
  (secretary/dispatch! "/masavi"))

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
       [masavi-input-row :description "Description" "text" data-set focus]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5 
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click masavi-form-cancel } "Cancel"]]]]]]]]])


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


(defn masavi-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get-auth (str serverhost "villages/search?name=" eval) onresp))))


(defn masavi-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [masavi-template
           "Masavi Add Form"
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
            "Masavi Update Form"
            update-data focus
            #(masavi-update-form-onclick update-data focus)])))


(defn masavi-update[id]
  (secretary/dispatch! (str "/masavi/update/" id)))

(defn masavi-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/masavi"))]
    (http-delete (str serverhost "masavis/" id)  onres)))

(defroute masavi-add-path "/masavi/add" []
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [masavi-add-template])))]
    (http-get-auth (str serverhost "villages") onres)))

(defroute masavi-upd-path "/masavi/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [masavi-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :masavis)))])))]
    (http-get-auth (str serverhost "villages") onres)))

(defn masavi-add [event]
  (secretary/dispatch! "/masavi/add"))

(defn render-masavi [masavis]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of Masavi Records"]]
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
          (when (is-admin-or-super-admin)[:th " "]) 
          (when (is-admin-or-super-admin)[:th " "])
          [:th "S.No"]
          [:th "Name of the Village"]
          [:th "Sub Division Name"]
          [:th "Tehsil"]
          [:th "Year"]
          [:th "Rack Number"]
          [:th "Description"]
         ]]
        [:tbody
         (doall (for [mt masavis]
                  ^{:key (.-id mt)} [:tr
                                     (when (is-admin-or-super-admin)[:td [button {:bs-style "success"
                                                                                  :on-click  #(masavi-update(.-id mt))} "Update"]])
                                     (when (is-admin-or-super-admin)[:td  [button {:bs-style "danger"
                                                                                   :on-click #(masavi-delete(.-id mt))} "Delete"]])
                                     [:td (.-serialnumber mt)]
                                     [:td (.-villagename mt)]
                                     [:td (.-subdivisionname mt)]
                                     [:td (.-tehsil mt)]
                                     [:td (.-year mt)]
                                     [:td (.-racknumber mt)]
                                     [:td (.-description mt)]
                                     ]))]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])


(defroute masavi-list "/masavi" []
  (if (nil? (get-value! :user)) (set-page! [login])
      (let [onres (fn [json]
                    (let [dt (getdata json)]
                      (set-key-value :masavis (.-data dt))
                      (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                      (set-page!  [render-masavi (get-value! :masavis)])))]
        (set-key-value :is-searched-results false)
        (http-get-auth (str serverhost "masavis?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres))))


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


(defn consolidation-form-cancel [event]
  (secretary/dispatch! "/consolidation"))

(defn consolidation-add [event]
  (secretary/dispatch! "/consolidation/add"))

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
       [consolidation-input-row :description "Description" "text" data-set focus]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5 
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click consolidation-form-cancel } "Cancel"]]]
        ]]]]]])


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


(defn consolidation-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get-auth (str serverhost "villages/search?name=" eval) onresp))))



(defn consolidation-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [consolidation-template
           "Consolidation Add Form"
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
            "Consolidation Update Form"
            update-data focus
            #(consolidation-update-form-onclick update-data focus)])))


(defn consolidation-update[id]
  (secretary/dispatch! (str "/consolidation/update/" id)))

(defn consolidation-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/consolidation"))]
    (http-delete (str serverhost "consolidations/" id)  onres)))

(defroute consolidation-add-path "/consolidation/add" []
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [consolidation-add-template])))]
    (http-get-auth (str serverhost "villages") onres)))

(defroute consolidation-upd-path "/consolidation/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [consolidation-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :consolidations)))])))]
    (http-get-auth (str serverhost "villages") onres)))

(defn render-consolidation [consolidations]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of Consolidation Records"]]
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
          (when (is-admin-or-super-admin)[:th " "]) 
          (when (is-admin-or-super-admin)[:th " "]) 
          [:th "S.No"]
          [:th "Name of the Village"]
          [:th "Sub Division Name"]
          [:th "Tehsil"]
          [:th "Year"]
          [:th "Rack Number"]
          [:th "Description"]
          ]]
        [:tbody
         (doall (for [mt consolidations]
                  ^{:key (.-id mt)} [:tr
                                     (when (is-admin-or-super-admin)[:td [button {:bs-style "success"
                                                                                  :on-click  #(consolidation-update(.-id mt))} "Update"]]) 
                                     (when (is-admin-or-super-admin)[:td  [button {:bs-style "danger"
                                                                                   :on-click #(consolidation-delete(.-id mt))} "Delete"]])
                                     [:td (.-serialnumber mt)]
                                     [:td (.-villagename mt)]
                                     [:td (.-subdivisionname mt)]
                                     [:td (.-tehsil mt)]
                                     [:td (.-year mt)]
                                     [:td (.-racknumber mt)]
                                     [:td (.-description mt)]
                                  ]))]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])

(defroute consolidation-list "/consolidation" []
  (if (nil? (get-value! :user)) (set-page! [login])
      (let [onres (fn [json]
                    (let [dt (getdata json)]
                      (set-key-value :consolidations (.-data dt))
                      (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                      (set-page!  [render-consolidation (get-value! :consolidations)])))]
        (set-key-value :is-searched-results false)
        (http-get-auth (str serverhost "consolidations?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres))))


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

(defn fieldbook-form-cancel [event]
  (secretary/dispatch! "/fieldbook"))

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
       [fieldbook-input-row :description "Description" "text" data-set focus]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5 
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click fieldbook-form-cancel } "Cancel"]]] ]]]]]])


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



(defn fieldbook-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get-auth (str serverhost "villages/search?name=" eval) onresp))))


(defn fieldbook-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [fieldbook-template
           "Field Book Add Form"
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
            "Field Book Update Form"
            update-data focus
            #(fieldbook-update-form-onclick update-data focus)])))


(defn fieldbook-update[id]
  (secretary/dispatch! (str "/fieldbook/update/" id)))

(defn fieldbook-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/fieldbook"))]
    (http-delete (str serverhost "fieldbooks/" id)  onres)))


(defroute fieldbook-add-path "/fieldbook/add" []
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [fieldbook-add-template])))]
    (http-get-auth (str serverhost "villages") onres)))

(defroute fieldbook-upd-path "/fieldbook/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [fieldbook-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :fieldbooks)))])))]
    (http-get-auth (str serverhost "villages") onres)))

(defn fieldbook-add [event]
  (secretary/dispatch! "/fieldbook/add"))
(defn render-fieldbook [fieldbooks]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of Field Book Records"]]
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
          (when (is-admin-or-super-admin) [:th " "])
          (when (is-admin-or-super-admin) [:th " "])
          [:th "S.No"]
          [:th "Name of the Village"]
          [:th "Sub Division Name"]
          [:th "Tehsil"]
          [:th "Year"]
          [:th "Rack Number"]
          [:th "Description"]
          ]]
        [:tbody
         (doall(for [mt fieldbooks]
                 ^{:key (.-id mt)} [:tr
                                    (when (is-admin-or-super-admin)[:td [button {:bs-style "success"
                                                                             :on-click  #(fieldbook-update(.-id mt))} "Update"]])
                                    (when (is-admin-or-super-admin)[:td  [button {:bs-style "danger"
                                                                                  :on-click #(fieldbook-delete(.-id mt))} "Delete"]])
                                    [:td (.-serialnumber mt)]
                                    [:td (.-villagename mt)]
                                    [:td (.-subdivisionname mt)]
                                    [:td (.-tehsil mt)]
                                    [:td (.-year mt)]
                                    [:td (.-racknumber mt)]
                                    [:td (.-description mt)]
                                   ]))]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])



(defroute fieldbook-list "/fieldbook" []
  (if (nil? (get-value! :user)) (set-page! [login])
      (let [onres (fn [json]
                    (let [dt (getdata json)]
                      (set-key-value :fieldbooks (.-data dt))
                      (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                      (set-page!  [render-fieldbook (get-value! :fieldbooks)])))]
        (set-key-value :is-searched-results false)
        (http-get-auth (str serverhost "fieldbooks?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres))))

;; ---------------------------------------------------------
;; misc-records

(defn misc-form-validator [data-set]
  (first (b/validate data-set
                     :filenumber [[v/required :message "Field is required"]]
                     :subject [[v/required :message "Field is required"]]
                     :title [[v/required :message "Field is required"]]
                     :remarks [[v/required :message "Field is required"]]
                     :dispatcheddate [[v/required :message "Field is required"]]
                     ;; :receiveddate [[v/required :message "Field is required"]]
                     )))

(defn misc-input-element [id ttype data-set placeholder in-focus bool]
  [:input.form-control {:id id
                        :type ttype
                        :value (@data-set id)
                        :placeholder placeholder
                        :on-change #(swap! data-set assoc id (-> % .-target .-value))
                        :on-blur  #(reset! in-focus "on")
                        :disabled bool
                        }])


(defn misc-input-row [id label ttype data-set focus bool]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [misc-input-element id ttype data-set label input-focus bool]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (misc-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((misc-form-validator @data-set) id)))]])
                        [:div])]])))


(defn misc-form-cancel [event]
  (secretary/dispatch! "/misc"))

(defn misc-template [doc-name data-set focus save-function bool]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [misc-input-row :filenumber "File Numbar" "text" data-set focus bool]
       [misc-input-row :subject "Subject" "text" data-set focus bool]
       [misc-input-row :title "Title" "text" data-set focus bool]
       [misc-input-row :remarks "Remarks" "text" data-set focus bool]
       [misc-input-row :dispatcheddate "Dispatched Date" "date" data-set focus bool]
       [misc-input-row :receiveddate "Recevied Date" "date" data-set focus false]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5 
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click misc-form-cancel } "Cancel"]]] ]]]]]])

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


(defn misc-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [misc-template
           "Misc Add Form"
           add-data
           focus
           #(misc-add-form-onclick add-data focus) false])))

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
    (fn []
      (if (is-admin-or-super-admin)
        [misc-template
         "Misc Update Form"
         update-data
         focus
         #(misc-update-form-onclick update-data focus) false]
        [misc-template
         "Misc Update Form"
         update-data
         focus
         #(misc-update-form-onclick update-data focus) true]))))



(defn misc-add [event]
  (secretary/dispatch! "/misc/add"))

(defn misc-update[id]
  (secretary/dispatch! (str "/misc/update/" id)))

(defn misc-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/misc"))]
    (http-delete (str serverhost "miscs/" id)  onres)))




(defroute misc-add-path "/misc/add" []
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [misc-add-template])))]
    (http-get-auth (str serverhost "villages") onres)))

(defroute misc-upd-path "/misc/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [misc-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :miscs)))])))]
    (http-get-auth (str serverhost "villages") onres)))

(defn render-misc [miscs]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of Misc Records"]]
    [:div.row
     [:div.col-md-12
      (when (is-admin-or-super-admin)
        [:div.form-group
         [:input {:type "button" :value "Add"
                  :class "btn btn-primary" :on-click misc-add}]
         ;; [:input {:id "getall" :type "button" :value "Refresh"
         ;;          :class "btn btn-primary" :on-click get-all-click}]
         ])
      [:div {:class "box-body"}
       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          [:th ""]
          (when (is-admin-or-super-admin) [:th " "])
          [:th "Fille Number"]
          [:th "Subject"]
          [:th "Title"]
          [:th "Remarks"]
          [:th "Dispatched Date"]
          [:th "Received Date"]
           ]]
        [:tbody
         (doall (for [mt miscs]
                  ^{:key (.-id mt)} [:tr
                                     [:td [button {:bs-style "success"
                                               :on-click  #(misc-update(.-id mt))} "Update"]] 
                                     (when (is-admin-or-super-admin)[:td  [button {:bs-style "danger"
                                                                                   :on-click #(misc-delete(.-id mt))} "Delete"]])
                                     [:td (.-filenumber mt)]
                                     [:td (.-subject mt)]
                                     [:td (.-title mt)]
                                     [:td (.-remarks mt)]
                                     [:td (.-dispatcheddate mt)]
                                     [:td (.-receiveddate mt)]]))]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])

(defroute misc-list "/misc" []
  (if (nil? (get-value! :user)) (set-page! [login])
      (let [onres (fn [json]
                    (let [dt (getdata json)]
                      (set-key-value :miscs (.-data dt))
                      (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                      (set-page!  [render-misc (get-value! :miscs)])))]
        (set-key-value :is-searched-results false)
        (http-get-auth (str serverhost "miscs?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres))))

;; ---------------------------------------------------------
;; o2register-records

(defn o2register-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber [[v/required :message "Field is required Must be number"]]
                     :subdivisionname [[v/required :message "Field is required"]]
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



(defn set-o2-fields [data-set mutdata]
  (do
    (swap! data-set assoc :subdivisionname (.-subdivisionname mutdata))
    (swap! data-set assoc :dateofinstitution (.-dateofinstitution mutdata))
    (swap! data-set assoc :nameofthefirstparty (.-nameofthefirstparty mutdata))
    (swap! data-set assoc :racknumber (.-nameofthefirstparty mutdata))
    (swap! data-set assoc :mutationid (int (.-id mutdata)))
    (swap! data-set assoc :villagename (.-villagename mutdata))))


(defn reset-o2-fields [data-set]
  (do
    (swap! data-set assoc :subdivisionname nil)
    (swap! data-set assoc :dateofinstitution nil)
    (swap! data-set assoc :nameofthefirstparty nil)
    (swap! data-set assoc :racknumber nil)
    (swap! data-set assoc :mutationid nil)
    (swap! data-set assoc :villagename "")
    (swap! data-set assoc :o2number nil)))

(defn on-o2-change [data-set]
  (let [selval (.-value (.getElementById js/document "O2-select"))
        onres (fn [json] (let [d (first (getdata json))]
                          (set-o2-fields data-set d)))]
    (if (= selval "select") (reset-o2-fields data-set)
        (http-get-auth (str serverhost "o2registers/"selval"/mutations")
                   onres))))

(defn o2-select
  [data-set]
  [:div.form-group
   [:label.col-sm-3.control-label "O2 Number"]
   [:div.col-sm-6
    [:select.form-control {:id "O2-select" :value (:o2number @data-set) :on-change #(on-o2-change data-set)}
     (for [d (get-value! :o2mutations)]
       ^{:key d} [:option {:value d} d])]]])


(defn o2register-form-cancel [event]
  (secretary/dispatch! "/o2register"))


(defn o2register-tags-template [id data-set]
  [:select.form-control {:id id :value (id @data-set) }
   (doall (for [d (get-value! :villages)]
            ^{:key (.-id d)} [:option {:value (.-villagename d)} (.-villagename d)]))])

(defn o2register-input-select [id label data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div#tagdiv.col-sm-6 [o2register-tags-template id  data-set]]
       [:div.col-sm-3 [:div]]])))

(defn o2register-template [doc-name data-set focus save-function]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [o2-select data-set]
       [o2register-input-int-row :serialnumber "Serial Number" "text" data-set focus]
       [o2register-input-row :subdivisionname "Sub Division Name" "text" data-set focus]
       [o2register-input-row :dateofinstitution "Date Of Institution" "date" data-set focus]
       [o2register-input-row :sourceofreceipt "Source Of Receipt" "text" data-set focus]
       [o2register-input-select :villagename "Village Name" data-set focus ]
       [o2register-input-row :nameofthefirstparty "Name Of The First Party" "text" data-set focus]
       [o2register-input-row :dateofreceiptfrompanchayat "Date Of Receipt From Panchayat" "date" data-set focus]
       [o2register-input-row :dateandgistoffinalorder "Date and Gist of Final Order" "date" data-set focus]
       [o2register-input-row :racknumber "Rack Number" "text" data-set focus]
       [o2register-input-int-row :startingyear "Starting Year" "text" data-set focus]
       [o2register-input-int-row :endingyear "Ending Year" "text" data-set focus]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5 
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click o2register-form-cancel } "Cancel"]]]
        ]]]]]])


(defn o2register-add-form-onclick [data-set focus]
  (reset! data-set (assoc @data-set :o2number (.-value (.getElementById js/document "O2-select"))))
  (if (= nil (o2register-form-validator @data-set))
    (let [onres (fn[json] (secretary/dispatch! "/o2register"))]
      (http-post (str serverhost "o2registers") onres  (.serialize (Serializer.) (clj->js @data-set))))
    (reset! focus "on")))


(defn o2register-update-form-onclick [data-set focus]
  (if (= nil (o2register-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :o2number  (.-value (.getElementById js/document "O2-select"))))
        (let [onres (fn[data] (secretary/dispatch! "/o2register"))]
          (http-put (str serverhost "o2registers/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on")))


(defn o2register-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get-auth (str serverhost "villages/search?name=" eval) onresp))))




(defn o2register-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [o2register-template
           "O2 Register Add Form"
           add-data focus
           #(o2register-add-form-onclick add-data focus)])))

(defn o2register-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :o2number (.-o2number dmt)
                             :serialnumber (.-serialnumber dmt)
                             :dateofinstitution (.-dateofinstitution dmt)
                             :villageid (.-villageid dmt)
                             :villagename (.-villagename dmt)
                             :subdivisionname (.-subdivisionname dmt)
                             :sourceofreceipt (.-sourceofreceipt dmt)
                             :nameofthefirstparty (.-nameofthefirstparty dmt)
                             :dateofreceiptfrompanchayat (.-dateofreceiptfrompanchayat dmt)
                             :dateandgistoffinalorder (.-dateandgistoffinalorder dmt)
                             :racknumber (.-racknumber dmt)
                             :startingyear (.-startingyear dmt)
                             :endingyear (.-endingyear dmt)
                             :mutationid (.-mutationid dmt)
                             })
        focus (r/atom nil)]
    (fn [] [o2register-template
           "O2 Register Update Form"
           update-data focus
           #(o2register-update-form-onclick update-data focus)])))


(defn o2register-update[id]
  (secretary/dispatch! (str "/o2register/update/" id)))

(defn o2register-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/o2register"))]
    (http-delete (str serverhost "o2registers/" id)  onres)))


(defroute o2register-add-path "/o2register/add" []
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [o2register-add-template])))
        ono2resp (fn [json] (set-key-value :o2mutations
                                          (clj->js (cons "select" (js->clj (getdata json))))))]
    (http-get-auth (str serverhost "mutations/o2numbers/search") ono2resp)
    (http-get-auth (str serverhost "villages") onres)))

(defroute o2register-upd-path "/o2register/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [o2register-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :o2registers)))])))
        ono2resp (fn [json] (set-key-value :o2mutations
                                          (clj->js (cons "select" (js->clj (getdata json))))))]
    (http-get-auth (str serverhost "mutations/o2numbers/search") ono2resp)
    (http-get-auth (str serverhost "villages") onres)))

(defn o2register-add [event]
  (secretary/dispatch! "/o2register/add"))

(defn render-o2register [o2registers]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of O2 Register Records"]]
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
          (when (is-admin-or-super-admin) [:th " "])
          (when (is-admin-or-super-admin) [:th " "])
          [:th "S.No"]
          [:th "Sub Division Name"]
          [:th "Date of Institution"]
          [:th "Source of Receipt"]
          [:th "Village Name"]
          [:th "Name of the First Party"]
          [:th "Date of Receipt"]
          [:th "Date of Gist Final Order"]
          [:th "Rack NUmber"]
          [:th "Starting Year"]
          [:th "Ending Year"]
         ]]
        [:tbody
         (doall (for [mt o2registers]
                  ^{:key (.-id mt)} [:tr
                                     (when (is-admin-or-super-admin)[:td [button {:bs-style "success"
                                                                                  :on-click  #(o2register-update(.-id mt))} "Update"]]) 
                                     (when (is-admin-or-super-admin)[:td  [button {:bs-style "danger"
                                                                                   :on-click #(o2register-delete(.-id mt))} "Delete"]])
                                     [:td (.-serialnumber mt)]
                                     [:td (.-subdivisionname mt)]
                                     [:td (.-dateofinstitution mt)]
                                     [:td (.-sourceofreceipt mt)]
                                     [:td (.-villagename mt)]
                                     [:td (.-nameofthefirstparty mt)]
                                     [:td (.-dateofreceiptfrompanchayat mt)]
                                     [:td (.-dateandgistoffinalorder mt)]
                                     [:td (.-racknumber mt)]
                                     [:td (.-startingyear mt)]
                                     [:td (.-endingyear mt)]
                ]))]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])

(defroute o2register-list "/o2register" []
  (if (nil? (get-value! :user)) (set-page! [login])
      (let [onres (fn [json]
                    (let [dt (getdata json)]
                      (set-key-value :o2registers (.-data dt))
                      (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                      (set-page! [render-o2register (get-value! :o2registers)])))]
        (set-key-value :is-searched-results false)
        (http-get-auth (str serverhost "o2registers?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres))))

;; ---------------------------------------------------------
;; o4register-records

(defn o4register-form-validator [data-set]
  (first (b/validate data-set
                     :subdivisionname [[v/required :message "Field is required"]]
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


(defn set-o4-fields [data-set mutdata]
  (do
    (swap! data-set assoc :subdivisionname (.-subdivisionname mutdata))
    (swap! data-set assoc :khatakhatuninumber (.-khatakhatuninumber mutdata))
    (swap! data-set assoc :khasranumber (.-khasranumber mutdata))
    (swap! data-set assoc :area (.-area mutdata))
    (swap! data-set assoc :mutationid (int (.-id mutdata)))
    (swap! data-set assoc :villagename (.-villagename mutdata))))


(defn reset-o4-fields [data-set]
  (do
    (swap! data-set assoc :subdivisionname nil)
    (swap! data-set assoc :khatakhatuninumber nil)
    (swap! data-set assoc :khasranumber nil)
    (swap! data-set assoc :area nil)
    (swap! data-set assoc :mutationid nil)
    (swap! data-set assoc :villagename "")
    (swap! data-set assoc :o4number nil)))

(defn on-o4-change [data-set]
  (let [selval (.-value (.getElementById js/document "O4-select"))
        onres (fn [json] (let [d (aget (getdata json) 0)]
                          (set-o4-fields data-set d)))]
    (if (= selval "select") (reset-o4-fields data-set)
        (http-get-auth (str serverhost "o4registers/"selval"/mutations")
                   onres))))


(defn o4-select
  [data-set]
  [:div.form-group
   [:label.col-sm-3.control-label "O4 Number"]
   [:div.col-sm-6
    [:select.form-control {:id "O4-select" :value (:o4number @data-set) :on-change #(on-o4-change data-set)}
     (for [d (get-value! :o4mutations)]
       ^{:key (.-id d)} [:option {:value (.-o4number d)} (.-o4number d)])]]])

(defn o4register-form-cancel [event]
  (secretary/dispatch! "/o4register"))


(defn o4register-template [doc-name data-set focus save-function]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [o4-select data-set]
       [o4register-input-row :subdivisionname "Sub Division Name" "text" data-set focus]
       [o4register-input-row :khatakhatuninumber "Khata Khatuni Number" "text" data-set focus]
       [o4register-input-row :numberanddateoforder "Number and Date of Order" "date" data-set focus]
       [o4register-input-row :khasranumber "Khasra Number" "text" data-set focus]
       [o4register-input-row :area "Area" "text" data-set focus]
       [o4register-input-row :revenuerentofshareofplotstransferred "Revenue Rent of Share of Plots Transfered" "text" data-set focus]
       [o4register-input-row :nameanddescriptionofthepersonsremoved "Name and Description of the Persons Removed" "text" data-set focus]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5 
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click o4register-form-cancel } "Cancel"]]] ]]]]]])


(defn o4register-add-form-onclick [data-set focus]
  (reset! data-set (assoc @data-set :o4number  (.-value (.getElementById js/document "O4-select"))))
  (if (= nil (o4register-form-validator @data-set))
    (let [onres (fn[json] (secretary/dispatch! "/o4register"))]
      (http-post (str serverhost "o4registers") onres  (.serialize (Serializer.) (clj->js @data-set))))
    (reset! focus "on")))


(defn o4register-update-form-onclick [data-set focus]
  (reset! data-set (assoc @data-set :o4number  (.-value (.getElementById js/document "O4-select"))))
  (if (= nil (o4register-form-validator @data-set))
    (let [onres (fn[data] (secretary/dispatch! "/o4register"))]
      (http-put (str serverhost "o4registers/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set))))
    (reset! focus "on")))

(defn o4register-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [o4register-template
           "O4 Register Add Form"
           add-data
           focus
           #(o4register-add-form-onclick add-data focus)])))

(defn o4register-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :o4number (.-o4number dmt)
                             :subdivisionname (.-subdivisionname dmt)
                             :khatakhatuninumber (.-khatakhatuninumber dmt)
                             :numberanddateoforder (.-numberanddateoforder dmt)
                             :khasranumber (.-khasranumber dmt)
                             :area (.-area dmt)
                             :revenuerentofshareofplotstransferred (.-revenuerentofshareofplotstransferred dmt)
                             :nameanddescriptionofthepersonsremoved (.-nameanddescriptionofthepersonsremoved dmt)
                             :villagename (.-villagename dmt)
                             :mutationid (.-mutationid dmt)
                             })
        focus (r/atom nil)]
    (fn [] [o4register-template
           "O4 Register Update Form"
           update-data
           focus
           #(o4register-update-form-onclick update-data focus)])))


(defn o4register-add [event]
  (secretary/dispatch! "/o4register/add"))

(defn o4register-update[id]
  (secretary/dispatch! (str "/o4register/update/" id)))

(defn o4register-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/o4register"))]
    (http-delete (str serverhost "o4registers/" id)  onres)))


(defroute o4register-add-path "/o4register/add" []
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [o4register-add-template])))
        ono4resp (fn [json] (set-key-value :o4mutations
                                          (clj->js (cons {:id 0 :o4number "select"} (js->clj (getdata json))))))]
    (http-get-auth (str serverhost "mutations/o4numbers/search") ono4resp)
    (http-get-auth (str serverhost "villages") onres)))

(defroute o4register-upd-path "/o4register/update/:id" [id]
  (let [ono4resp (fn [json] (set-key-value :o4mutations
                                          (clj->js (cons {:id 0 :o4number "select"} (js->clj (getdata json))))))]
    (http-get-auth (str serverhost "mutations/o4numbers/search") ono4resp)
    (set-page! [o4register-update-template id
                (first (filter (fn[obj]
                                 (=(.-id obj) (.parseInt js/window id))) (get-value! :o4registers)))])))



(defn render-o4register [o4registers]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of O4 Register Records"]]
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
          (when (is-admin-or-super-admin) [:th " "])
          (when (is-admin-or-super-admin) [:th " "])
          [:th "Sub Division Name"]
          [:th "Khata Khatuni Number"]
          [:th "Number and Date of Order"]
          [:th "Khasra Number"]
          [:th "Area"]
          [:th "Revenue of Share of Plots Transfered"]
          [:th "Name and Description of the Persons Removed"]
          ]]
        [:tbody
         (doall (for [mt o4registers]
                  ^{:key (.-id mt)} [:tr
                                     (when (is-admin-or-super-admin)[:td [button {:bs-style "success"
                                                                                  :on-click  #(o4register-update(.-id mt))} "Update"]]) 
                                     (when (is-admin-or-super-admin)[:td  [button {:bs-style "danger"
                                                                                   :on-click #(o4register-delete(.-id mt))} "Delete"]])
                                     [:td (.-subdivisionname mt)]
                                     [:td (.-khatakhatuninumber mt)]
                                     [:td (.-numberanddateoforder mt)]
                                     [:td (.-khasranumber mt)]
                                     [:td (.-area mt)]
                                     [:td (.-revenuerentofshareofplotstransferred mt)]
                                     [:td (.-nameanddescriptionofthepersonsremoved mt)]
                                      ]))]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])




(defroute o4register-list "/o4register" []
  (if (nil? (get-value! :user)) (set-page! [login])
      (let [onres (fn [json]
                    (let [dt (getdata json)]
                      (set-key-value :o4registers (.-data dt))
                      (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                      (set-page!  [render-o4register (get-value! :o4registers)])))]
        (set-key-value :is-searched-results false)
        (http-get-auth (str serverhost "o4registers?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres))))


;; ---------------------------------------------------------
;; o6register-records

(defn o6register-form-validator [data-set]
  (first (b/validate data-set
                     :subdivisionname [[v/required :message "Field is required"]]
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


(defn set-o6-fields [data-set mutdata]
  (do
    (swap! data-set assoc :subdivisionname (.-subdivisionname mutdata))
    (swap! data-set assoc :mutationid (int (.-id mutdata)))
    (swap! data-set assoc :villagename (.-villagename mutdata))))


(defn reset-o6-fields [data-set]
  (do
    (swap! data-set assoc :subdivisionname nil)
    (swap! data-set assoc :mutationid nil)
    (swap! data-set assoc :villagename "")
    (swap! data-set assoc :o6number nil)))

(defn on-o6-change [data-set]
  (let [selval (.-value (.getElementById js/document "O6-select"))
        onres (fn [json] (let [d (first (getdata json))]
                          (set-o6-fields data-set d)))]
    (if (= selval "select") (reset-o6-fields data-set)
        (http-get-auth (str serverhost "o6registers/"selval"/mutations")
                       onres))))


(defn o6-select
  [data-set]
  [:div.form-group
   [:label.col-sm-3.control-label "O6 Number"]
   [:div.col-sm-6
    [:select.form-control {:id "O6-select" :value (:o6number @data-set) :on-change #(on-o6-change data-set)}
     (for [d (get-value! :o6mutations)]
       ^{:key (.-id d)} [:option {:value (.-o6number d)} (.-o6number d)])]]])


(defn o6register-form-cancel [event]
  (secretary/dispatch! "/o6register"))


(defn o6register-tags-template [data-set]
  [:select.form-control {:id "o6register-districts" :value (:villagename @data-set)}
   (doall (for [d (get-value! :villages)]
            ^{:key (.-id d)} [:option {:value (.-villagename d)} (.-villagename d)]))])

(defn o6register-input-select [label data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div#tagdiv.col-sm-6 [o6register-tags-template data-set]]
       [:div.col-sm-3 [:div]]])))

(defn o6register-template [doc-name data-set focus save-function]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [o6-select data-set]
       [o6register-input-row :subdivisionname "Sub Division Name" "text" data-set focus]
       [o6register-input-row :year "Year" "date" data-set focus]
       [o6register-input-row :mehsilnumber "Mehsil Number" "text" data-set focus]
       [o6register-input-row :dateoforderlevy "Date of Order Levy" "date" data-set focus]
       [o6register-input-select "Village Name" data-set focus ]
       [o6register-input-row :nameofpersonwhomrecoveryismade "Name of Person Whom Recovery is Made" "text" data-set focus]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5 
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click o6register-form-cancel } "Cancel"]]]
        ]]]]]])


(defn o6register-add-form-onclick [data-set focus]
  (if (= nil (o6register-form-validator @data-set))
    (do
      (reset! data-set (assoc @data-set :o6number (.-value (.getElementById js/document "O6-select"))))
      (let [onres (fn[json] (secretary/dispatch! "/o6register"))]
        (http-post (str serverhost "o6registers") onres  (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on")))


(defn o6register-update-form-onclick [data-set focus]
  (if (= nil (o6register-form-validator @data-set))
    (do (reset! data-set (assoc @data-set :o6number (.-value (.getElementById js/document "O6-select"))))
      ;;  (js/console.log (clj->js @data-set))
        (let [onres (fn[data] (secretary/dispatch! "/o6register"))]
          (http-put (str serverhost "o6registers/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
    (reset! focus "on")))

(defn o6register-on-change [event]
  (let [ele (.getElementById js/document "id")
        eval (.-value ele)
        onresp (fn [json]
                 (let [dt (getdata json)]
                   (set-key-value :villages dt)))]
    (set-key-value :villages [])
    (when-not ( >  1 (.-length eval))
      (http-get-auth (str serverhost "villages/search?name=" eval) onresp))))


(defn o6register-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [o6register-template
           "O6 Register Add Form"
           add-data focus
           #(o6register-add-form-onclick add-data focus)])))

(defn o6register-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :mutationid (.-mutationid dmt)
                             :o6number (.-o6number dmt)
                             :subdivisionname (.-subdivisionname dmt)
                             :year (.-year dmt)
                             :mehsilnumber (.-mehsilnumber dmt)
                             :dateoforderlevy (.-dateoforderlevy dmt)
                             :villageid (.-villageid dmt)
                             :villagename (.-villagename dmt)
                             :nameofpersonwhomrecoveryismade (.-nameofpersonwhomrecoveryismade dmt)})
        focus (r/atom nil)]
    (fn [] [o6register-template
           "O6 Register Update Form"
           update-data focus
           #(o6register-update-form-onclick update-data focus)])))


(defn o6register-update[id]
  (secretary/dispatch! (str "/o6register/update/" id)))

(defn o6register-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/o6register"))]
    (http-delete (str serverhost "o6registers/" id)  onres)))

(defroute o6register-add-path "/o6register/add" []
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [o6register-add-template])))
        ono6resp (fn [json] (set-key-value :o6mutations
                                          (clj->js (cons {:id 0 :o6number "select"} (js->clj (getdata json))))))]
    (http-get-auth (str serverhost "mutations/o6numbers/search") ono6resp)
    (http-get-auth (str serverhost "villages") onres)))

(defroute o6register-upd-path "/o6register/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [o6register-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :o6registers)))])))
        ono6resp (fn [json] (set-key-value :o6mutations
                                          (clj->js (cons {:id 0 :o6number "select"} (js->clj (getdata json))))))]
    (http-get-auth (str serverhost "mutations/o6numbers/search") ono6resp)
    (http-get-auth (str serverhost "villages") onres)))

(defn o6register-add [event]
  (secretary/dispatch! "/o6register/add"))

(defn render-o6register [o6registers]
  [:div
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3 "List of O6 Register Records"]]
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
          (when (is-admin-or-super-admin) [:th " "])
          (when (is-admin-or-super-admin) [:th " "])
          [:th "Sub Divison Name"]
          [:th "Year"]
          [:th  "Mehsil Number"]
          [:th "Date of Orederlevy"]
          [:th "Village Name"]
          [:th "name of Person Whom Recovery is Made"]
         
          ]]
        [:tbody
         (doall (for [mt o6registers]
                  ^{:key (.-id mt)} [:tr
                                     (when (is-admin-or-super-admin)[:td [button {:bs-style "success"
                                                                                  :on-click  #(o6register-update(.-id mt))} "Update"]])
                                     (when (is-admin-or-super-admin)[:td  [button {:bs-style "danger"
                                                                                   :on-click #(o6register-delete(.-id mt))}"Delete"]])
                                     [:td (.-subdivisionname mt)]
                                     [:td (.-year mt)]
                                     [:td (.-mehsilnumber mt)]
                                     [:td (.-dateoforderlevy mt)]
                                     [:td (.-villagename mt)]
                                     [:td (.-nameofpersonwhomrecoveryismade mt)]
                                      ]))]]
       [:div{:class "col-xs-6 col-centered col-max"} [shared-state 0]]]]]]])




(defroute o6register-list "/o6register" []
  (if (nil? (get-value! :user)) (set-page! [login])
      (let [onres (fn [json]
                    (let [dt (getdata json)]
                      (set-key-value :o6registers (.-data dt))
                      (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                      (set-page!  [render-o6register (get-value! :o6registers)])))]
        (set-key-value :is-searched-results false)
        (http-get-auth (str serverhost "o6registers?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres))))

;; ----------------------------------------------------------------------------------


(defn table-mount []
  (.ready (js/$ js/document)
          (fn []
            (.DataTable (js/$ "#example1")))))
(defn home [documents]
  (r/create-class {:reagent-render render-mutations
                   :component-did-mount table-mount }))


(defroute home-path "/" []
  (if (nil? (get-value! :user)) (set-key-value :page-location [login])
      (let [onres (fn [json]
                    (let [dt (getdata json)]
                      (set-key-value :mutations (.-data dt))
                      (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
                      (set-page! [render-mutations (get-value! :mutations)])))
            dist-res (fn[json] (set-key-value :districts (getdata json)))]
        (do
          (set-key-value :is-searched-results false)
          (http-get-auth (str serverhost "mutations?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)
          (http-get-auth (str serverhost "districts") dist-res)))))

(defroute "*" []
  (js/alert "<h1>Not Found Page</h1>"))


(defn main
  []
  (secretary/set-config! :prefix "#")
  (set-login-page)
  (set-key-value :page-location [login])
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
