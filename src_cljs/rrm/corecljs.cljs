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
            [bouncer.validators :as v]
            [hodgepodge.core :refer [local-storage
                                     get-item
                                     set-item
                                     remove-item
                                     clear!
                                     length]]
            [cljs.reader :as reader]
            [accountant.core :as accountant])
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
                          :mutationnumbers nil
                          :message-client nil
                          :message-server nil}))

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

(defn set-url [url]
  (let [ls (reader/read-string (js->clj (get-item local-storage "session")))]
    (set-item local-storage "session" (assoc ls :current-url url))))


;; ----------------------------------------------------------------------------------
;; login-form with validations

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
                                (set-item local-storage "session" {:current-url "/" :user-info
                                                                   {:token (get-value! :token)
                                                                    :user (get-value! :user)}})
                                (reset-login-page)
                                (accountant/navigate! "/"))))]
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



;; end of login-form
;; ------------------------------------------------------------------

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
  (remove-item local-storage "session")
  ;;(set-page! [login])
  (accountant/navigate! "/login"))

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

(defn set-authorized-list [json list-store-key
                           list-page]
  (let [dt (getdata json)]
    (set-key-value list-store-key (.-data dt))
    (set-key-value :total-pages (get-total-rec-no (.-pagesCount dt)))
    (set-page! [list-page (get-value! list-store-key)])))

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
                (cond (= (get-status json) 200) (set-authorized-list json :mutations
                                                                     render-mutations)
                      :esle (sign-out)))]
    (http-get-auth (get-index-url (get-value! :is-searched-results)
                                  (dec (get-value! :current-page))
                                  vid mn fp sp po
                                  kknum so2 st knum
                                  so4 so6) onres)))

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
  (accountant/navigate! "/"))

;;...... Side Tab Events .........

(defn mut-click [event]
  (accountant/navigate! "/"))

(defn rev-click [event]
  (accountant/navigate! "/revenue"))

(defn khr-click [event]
  (accountant/navigate! "/khasragirdwani"))

(defn masavi-click [event]
  (accountant/navigate! "/masavi"))

(defn cons-click [event]
  (accountant/navigate! "/consolidation"))

(defn field-click [event]
  (accountant/navigate! "/fieldbook"))

(defn misc-click [event]
  (accountant/navigate! "/misc"))

(defn o2-click [event]
  (accountant/navigate! "/o2register"))

(defn o4-click [event]
  (accountant/navigate! "/o4register"))

(defn o6-click [event]
  (accountant/navigate! "/o6register"))

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
        onres (fn [json] (cond (= (get-status json) 200)(let [data (getdata json)]
                                                         (if (empty? (.-data data))
                                                           (do
                                                             (set-key-value :message-server "No Records to Display")
                                                             (set-key-value :message-client nil)
                                                             (set-authorized-list json :mutations render-mutations))
                                                           (do
                                                             (set-key-value :message-server nil)
                                                             (set-key-value :message-client nil)
                                                             (set-authorized-list json :mutations render-mutations))))
                              :else (sign-out)))]
    (if (and (empty? mn) (empty? st) (= "0" vid) (empty? po) (empty? so2) (empty? so4) (empty? so6) (empty? knum) (empty? kknum) (empty? fp) (empty? sp))
      (do
        (set-key-value :message-client "Please Enter Any One Field")
        (set-key-value :message-server nil)
        (set-key-value :mutations nil)
        (set-key-value :total-pages nil)
        (set-page! [render-mutations (get-value! :mutations)] ))
      (do
        (set-key-value :message-client nil)
        (set-key-value :current-page 1)
        (set-key-value :is-searched-results true)
        (http-get (str (get-search-url
                        vid mn fp sp po
                        kknum so2 st knum so4 so6)"&pageIndex=0&pageSize=10") onres)))))

;; --------------------------------------------------------------------------
;; mutation form


(defn date-year-after? [date]
  (let [year (js/parseInt (re-find #"\d+" date))]
    (if (> year 999)
      (tt/after? (tt/date-time year) (tt/date-time 1900))
      false)))

(defn date-year-before? [date]
  (let [year (js/parseInt (re-find #"\d+" date))]
    (if (< year 999999)
      (tt/before?  (tt/date-time year) (tt/date-time 9999))
      false)))

(defn year-after? [date]
  (let [year (js/parseInt date)]
    (if (> year 999)
      (tt/after? (tt/date-time year) (tt/date-time 1900))
      false)))

(defn year-before? [date]
  (let [year (js/parseInt date)]
    (if (< year 999999)
      (tt/before?  (tt/date-time year) (tt/date-time 9999))
      false)))


(defn form-validator [data-set]
  (first (b/validate data-set
                       :mutationnumber [[v/required :message "Field is required"]]
                       :nameofthefirstparty [[v/required :message "Field is required"]]
                       :nameofthesecondparty [[v/required :message "Field is required"]]
                       :dateofinstitution [[v/required :message "Field is required"]
                                           [date-year-after? :message "Year must greater than 1900"]
                                           [date-year-before? :message "Year must less than 9999"]]
                       :nameofpo [[v/required :message "Field is required"]]
                       :dateofdecision [[v/required :message "Field is required"]
                                        [date-year-after? :message "Year must greater than 1900"]
                                        [date-year-before? :message "Year must less than 9999"]]
                       :title [[v/required :message "Field is required"]]
                       :khasranumber [[v/required :message "Field is required"]]
                       :khatakhatuninumber [[v/required :message "Field is required"]]
                       :racknumber [[v/required :message "Field is required"]]
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
  (accountant/navigate! "/"))

(defn date-input [id data-set placeholder bool focus-on]
  [:input.form-control {:id id
                        :type "date"
                        :value (@data-set id)
                        :placeholder placeholder
                        :on-change #(swap! data-set assoc id (-> % .-target .-value))
                        :on-blur #(reset! focus-on "true" )
                        :disabled bool
                        }])

(defn senddate-validator [data-set]
  (first (b/validate data-set
                     :senddate [[v/required :message "Field is required"]
                                [date-year-after? :message "Year must greater than 1900"]
                                [date-year-before? :message "Year must less than 9999"]])))

(defn add-check-input-element [id label data-set focus bool]
  (let [check (r/atom true)
        focus-on (r/atom nil)]
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
           [:div.col-sm-6 [date-input id data-set label false focus-on]]
           [:div.col-sm-3 (if @focus-on
                            (if (= nil (senddate-validator @data-set))
                              [:div]
                              [:div {:style  {:color "red"}}
                               [:b (str (first ((senddate-validator @data-set) id)))]])
                            [:div])]]
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
       [form-input-element :title "Nature of Case" "text" data-set focus false]
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
         [button {:bs-style "danger" :on-click form-cancel } "Cancel"]]]]
       ;; [:span (str @data-set)]
       ]]]])

(defn receiveddate-validator [data-set]
  (first (b/validate data-set
                     :receiveddate [[v/required :message "Field is required"]
                                    [date-year-after? :message "Year must greater than 1900"]
                                    [date-year-before? :message "Year must less than 9999"]])))


(defn upd-check-input-element [data-set focus bool ]
  (let [check (r/atom true)
        focus-on (r/atom nil)
        focus-on1 (r/atom nil)]
    (fn []
      [:div
       [:div.form-group
        [:label.col-sm-3.control-label "Send Date"]
        [:div.col-sm-6 [date-input :senddate data-set "Send Date" true focus-on]]
        [:div.col-sm-3  (if @focus-on
                          (if (= nil (senddate-validator @data-set))
                            [:div]
                            [:div {:style  {:color "red"}}
                             [:b (str (first ((senddate-validator @data-set) :senddate)))]])
                          [:div])]]
       (if (:receiveddate @data-set)
         [:div
          [:div.form-group
           [:label.col-sm-3.control-label "Received Date"]
           [:div.col-sm-6 [date-input :receiveddate data-set "Received Date" false focus-on1]]
           [:div.col-sm-3   (if @focus-on1
                              (if (= nil (receiveddate-validator @data-set))
                                [:div]
                                [:div {:style  {:color "red"}}
                                 [:b (str (first ((receiveddate-validator @data-set) :receiveddate)))]])
                              [:div])]]
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
              [:div.col-sm-6 [date-input :receiveddate data-set "Received Date" false focus-on1]]
              [:div.col-sm-3 (if @focus-on1
                               (if (= nil (receiveddate-validator @data-set))
                                 [:div]
                                 [:div {:style  {:color "red"}}
                                  [:b (str (first ((receiveddate-validator @data-set) :receiveddate)))]])
                               [:div])]]])
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
       [form-input-element :title "Nature of Case" "text" data-set focus false]
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
         [button {:bs-style "danger" :on-click form-cancel } "Cancel"]]]]
       ;; [:span (str @data-set)]
       ]]]])


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
  (if (= nil (@data-set :senddate))
    (if (= nil (form-validator @data-set))
      (let [onres (fn[json] (swap! data-set dissoc
                                 :mutationnumber  :khasranumber  :khatakhatuninumber
                                 :nameofthesecondparty :nameofthefirstparty
                                 :nameofpo :o6number :o2number :o4number
                                 :title :dateofdecision :dateofinstitution
                                 :racknumber :area :senddate
                                 :remarks
                                 ))]
        (http-post (str serverhost "mutations") onres  (.serialize (Serializer.) (clj->js (map-mutation-data @data-set)))))
      (reset! focus "on"))
    (when (= nil (form-validator @data-set) (senddate-validator @data-set))
      (let [onres (fn[json] (swap! data-set dissoc
                                   :mutationnumber  :khasranumber  :khatakhatuninumber
                                   :nameofthesecondparty :nameofthefirstparty
                                   :nameofpo :o6number :o2number :o4number
                                   :title :dateofdecision :dateofinstitution
                                   :racknumber :area :senddate
                                   :remarks
                                   ))]
        (http-post (str serverhost "mutations") onres  (.serialize (Serializer.) (clj->js (map-mutation-data @data-set))))))))

(defn update-form-onclick [data-set focus]
  (if (= nil (@data-set :senddate) (@data-set :receiveddate))
    (if (= nil (form-validator @data-set))
      (let [onres (fn[data]
                    (accountant/navigate! "/"))]
        (when (and (not (nil? (:senddate @data-set))) (nil? (:receiveddate @data-set)))
          (swap! data-set assoc :racknumber ""))
        (http-put (str serverhost "mutations/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js (map-mutation-data @data-set)))))
      (reset! focus "on"))
    (if (and (not (nil? (@data-set :senddate))) (= nil (@data-set :receiveddate)))
      (if (= nil (form-validator @data-set) (senddate-validator @data-set))
        (let [onres (fn[data]
                      (accountant/navigate! "/"))]
          (when (and (not (nil? (:senddate @data-set))) (nil? (:receiveddate @data-set)))
            (swap! data-set assoc :racknumber ""))
          (http-put (str serverhost "mutations/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js (map-mutation-data @data-set)))))
        (reset! focus "on"))
      (when (and (not (nil? (@data-set :receiveddate))) (not (nil? (@data-set :receiveddate))))
        (if (= nil (form-validator @data-set) (receiveddate-validator @data-set))
          (let [onres (fn[data]
                        (accountant/navigate! "/"))]
            (when (and (not (nil? (:senddate @data-set))) (nil? (:receiveddate @data-set)))
              (swap! data-set assoc :racknumber ""))
            (http-put (str serverhost "mutations/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js (map-mutation-data @data-set)))))
          (reset! focus "on"))))))

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
  (accountant/navigate! (str "/mutations/update/" id)))

(defn delete[id]
  (let [del-conf? (js/confirm "Are you sure you want to delete?")
        onres (fn [json]
                (accountant/navigate! "/"))]
    (when del-conf? (http-delete (str serverhost "mutations/" id)  onres))))

(defn add [event]
  (accountant/navigate! "/mutations/add"))


(defn get-all-click [event]
  (let [onres (fn [json]
                (let [mt (getdata json)]
                  (set-key-value :mutations (.-data mt))
                  (set-key-value :total-pages (get-total-rec-no
                                               (.-pagesCount mt)))
                  (set-key-value :current-page 1)
                  (set-page! [render-mutations (get-value! :mutations)])))]
    (do
      (set-key-value :is-searched-results false)
      (set-key-value :message-client nil)
      (set-key-value :message-server nil)
      (set! (.-value (dom/getElement "mutationnumber")) "")
      (set! (.-value (dom/getElement "stitle")) "")
      (set! (.-value (dom/getElement "src-vill")) 0)
      (set! (.-value (dom/getElement "src-sub")) 0)
      (set! (.-value (dom/getElement "src-dist")) 0)
      (set! (.-value (dom/getElement "svillagename")) "")
      (set! (.-value (dom/getElement "so2number")) "")
      (set! (.-value (dom/getElement "so4number")) "")
      (set! (.-value (dom/getElement "so6number")) "")
      (set! (.-value (dom/getElement "skhasranumber")) "")
      (set! (.-value (dom/getElement "skhatakhatuninumber")) "")
      (set! (.-value (dom/getElement "snameofthefirstparty")) "")
      (set! (.-value (dom/getElement "snameofthesecondparty")) "")
      (http-get-auth (str serverhost "mutations?pageIndex=0&pageSize=10") onres))))

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
    [:div.col-md-offset-5
     [:h3 "Mutation Details"]]
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
        [:label "Nature of Case"]
        [:input.form-control {:id "stitle"
                              :type "text"
                              :placeholder "Nature of Case"}]]
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
       [:div.col-sm-4
        [button-tool-bar
         [button {:bs-style "primary"  :on-click search } "Search"]
         [button {:id "getall" :bs-style "primary" :on-click get-all-click} "Refresh"]]]
        [:div.col-sm-4
         (when (:message-client @storage)
           [:div.alert.alert-danger [:center [:b [:i.icon.fa.fa-ban] (str (:message-client @storage))]]])
         (when (:message-server @storage)
           [:div.alert.alert-danger [:center [:b [:i.icon.fa.fa-ban] (str (:message-server @storage))]]])]]]
     ]]

   [:div.box
    [:div.box-header
     [:h3.box-title "List of Mutations"]]
    [:div {:class "box-body"}
     [:div.form-group
      [:div.row
       [:div.col-sm-10
        [button-tool-bar
         [button {:bs-style "primary"  :on-click add} "Add"]]]]]
     [:div.table-responsive
     [:table {:class "table table-bordered table-striped dataTable"}
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
     [:div.col-sm-6.col-md-offset-5 [shared-state 0]]]]])


(defroute mutations-list "/mutations" []
  (let [onres (fn [json]
                (cond (= (get-status json) 200) (set-authorized-list json :mutations render-mutations)
                      :else (sign-out)))]
    (set-key-value :is-searched-results false)
    (http-get-auth (str serverhost "mutations?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


(defroute documents-path "/mutations/add" []
  (if (nil? (get-value! :user)) (accountant/navigate! "/login")
      (let [onres (fn[json](
                           (set-key-value :districts (getdata json))
                           (set-page! [mutation-add-template])))]
        (set-url "/mutations/add")
        (http-get-auth (str serverhost "districts") onres))))

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
                     :year [[v/required :message "Field is required Must be number"]
                            [year-after? :message "Year must greater than 1900"]
                            [year-before? :message "Year must less than 9999"]]
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
  (accountant/navigate! "/revenue"))

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
       [revenue-input-int-row :year "Year" "text" data-set focus]
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
    (let [onres (fn[json] (accountant/navigate! "/revenue"))]
      (http-post (str serverhost "revenuerecords") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on"))


(defn revenue-update-form-onclick [data-set focus]
  (if (= nil (revenue-form-validator @data-set))
    (let [onres (fn[data] (accountant/navigate! "/revenue"))]
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
  (accountant/navigate! (str "/revenue/update/" id)))

(defn revenue-delete[id]
  (let [onres (fn [json]
                (accountant/navigate! "/revenue"))]
    (http-delete (str serverhost "revenuerecords/" id)  onres)))


(defroute revenue-add-path "/revenue/add" []
  (if (nil? (get-value! :user)) (accountant/navigate! "/login")
      (let [onres (fn[json](
                           (set-key-value :subdivisions (getdata json))
                           (set-page! [revenue-add-template])))]
        (http-get-auth (str serverhost "subdivisions") onres))))

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
  (accountant/navigate! "/revenue/add"))



(defn render-revenue [revenues]
  [:div.col-md-12
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3.box-title "List of Revenue Records"]]
    [:div.box-body
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click revenue-add}]]
      [:div.table-responsive
       [:table {:class "table table-bordered table-striped dataTable"}
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
                  ^{:key (.-id mt)}
                  [:tr
                   (when (is-admin-or-super-admin)
                     [:td [button {:bs-style "success"
                                   :on-click  #(revenue-update(.-id mt))} "Update"]])
                   (when (is-admin-or-super-admin)
                     [:td  [button {:bs-style "danger"
                                    :on-click #(revenue-delete(.-id mt))} "Delete"]])
                   [:td (.-serialnumber mt)]
                   [:td (.-subdivisionname mt)]
                   [:td (.-villagename mt)]
                   ;; [:td (.-tehsil mt)]
                   [:td (.-year mt)]
                   [:td (.-racknumber mt)]
                   [:td (.-description mt)]
                   ]))]]]
       [:div.col-sm-6.col-md-offset-5 [shared-state 0]]]]])


(defroute revenue-list "/revenue" []
  (let [onres (fn [json]
                (cond (= (get-status json) 200) (set-authorized-list json :revenues render-revenue)
                      :else (sign-out)))]
    (set-url "/revenue")
    (set-key-value :is-searched-results false)
    (http-get-auth (str serverhost "revenuerecords?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


;; ---------------------------------------------------------
;; khasragirdwani-records

(defn khasragirdwani-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber [[v/required :message "Field is required Must be number"]]
                     :year [[v/required :message "Field is required Must be number"]
                            [year-after? :message "Year must greater than 1900"]
                            [year-before? :message "Year must less than 9999"]]
                     :racknumber [[v/required :message "Field is required"]]
                     :description [[v/required :message "Field is required"]])))

(defn khasragirdwani-input-int [id ttype data-set placeholder in-focus]
  [:input.form-control {:id id
                        :type ttype
                        :value (@data-set id)
                        :placeholder placeholder
                        :on-change #(swap! data-set assoc id (int (-> % .-target .-value )))
                        :on-blur  #(reset! in-focus "on")
                        }])


(defn khasragirdwani-input-int-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [khasragirdwani-input-int id ttype data-set label input-focus]]
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


(defn kg-sub-onchange [id val data-set]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (set-key-value :villages dt)))]
    (do (http-get-auth (str serverhost "subdivisions/" val  "/villages") res)
        (swap! data-set assoc id (js/parseInt val)))))

(defn kg-sub-sel-tag [id data data-set]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(kg-sub-onchange  id (-> % .-target .-value) data-set)}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-subdivisionname d)])])

(defn kg-form-sub-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [kg-sub-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])


(defn kg-villages-sel-tag [id data data-set ]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(swap! data-set assoc id (js/parseInt (-> % .-target .-value)))}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-villagename d)])])

(defn kg-form-villages-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [kg-villages-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])

(defn khasragirdwani-form-cancel [event]
  (accountant/navigate! "/khasragirdwani"))

(defn khasragirdwani-template [doc-name data-set focus save-function]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [khasragirdwani-input-int-row :serialnumber "Serial Number" "text" data-set focus]
       [kg-form-sub-sel "Sub Division Name" :subdivisionid (:subdivisions @storage) data-set]
       [kg-form-villages-sel "Village Name" :villageid (:villages @storage) data-set]
       [khasragirdwani-input-int-row :year "Year" "text" data-set focus]
       [khasragirdwani-input-row :racknumber "Rack Number" "text" data-set focus]
       [khasragirdwani-input-row :description "Description" "text" data-set focus]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click khasragirdwani-form-cancel } "Cancel"]]]
        ]]]]]])



(defn khasragirdwani-add-form-onclick [data-set focus]
  (if (= nil (revenue-form-validator @data-set))
    (let [onres (fn[json] (accountant/navigate! "/khasragirdwani"))]
      (http-post (str serverhost "khasragirdwanis") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on"))


(defn khasragirdwani-update-form-onclick [data-set focus]
  (if (= nil (khasragirdwani-form-validator @data-set))
    (let [onres (fn[data] (accountant/navigate! "/khasragirdwani"))]
      (http-put (str serverhost "khasragirdwanis/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on"))



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
           "Khasra Girdwani Add Form"
           add-data focus
           #(khasragirdwani-add-form-onclick add-data focus)])))

(defn khasragirdwani-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :serialnumber (.-serialnumber dmt)
                             :subdivisionid (.-subdivisionid dmt)
                             :villageid (.-villageid dmt)
                             :year (.-year dmt)
                             :racknumber (.-racknumber dmt)
                             :description (.-description dmt)
                             :isactive true})
        focus (r/atom nil)]
    (fn [] [khasragirdwani-template
           "KhasraGirdwani Update Form"
           update-data focus
           #(khasragirdwani-update-form-onclick update-data focus)])))


(defn khasragirdwani-update[id]
  (accountant/navigate! (str "/khasragirdwani/update/" id)))

(defn khasragirdwani-delete[id]
  (let [onres (fn [json]
                (accountant/navigate! "/khasragirdwani"))]
    (http-delete (str serverhost "khasragirdwanis/" id)  onres)))


(defroute khasragirdwani-add-path "/khasragirdwani/add" []
  (if (nil? (get-value! :user)) (accountant/navigate! "/login")
      (let [onres (fn[json](
                           (set-key-value :subdivisions (getdata json))
                           (set-page! [khasragirdwani-add-template])))]
        (http-get-auth (str serverhost "subdivisions") onres))))

(defroute khasragirdwani-upd-path "/khasragirdwani/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [khasragirdwani-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :khasragirdwanis)))])))
        sub-res (fn[json](set-key-value :subdivisions (getdata json)))]
    (do
      (http-get-auth (str serverhost "villages") onres)
      (http-get-auth (str serverhost "subdivisions") sub-res))))

(defn khasragirdwani-add [event]
  (accountant/navigate! "/khasragirdwani/add"))



(defn render-khasragirdwani [khasragirdwanis]
  [:div.col-md-12
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3.box-title "List of Khasragirdwani"]]
    [:div.box-body
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click khasragirdwani-add}]]
      [:div.table-responsive
       [:table {:class "table table-bordered table-striped dataTable"}
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
         (doall (for [mt khasragirdwanis]
                  ^{:key (.-id mt)}
                  [:tr
                   (when (is-admin-or-super-admin)
                     [:td [button {:bs-style "success"
                                   :on-click  #(khasragirdwani-update(.-id mt))} "Update"]])
                   (when (is-admin-or-super-admin)
                     [:td  [button {:bs-style "danger"
                                    :on-click #(khasragirdwani-delete(.-id mt))} "Delete"]])
                   [:td (.-serialnumber mt)]
                   [:td (.-subdivisionname mt)]
                   [:td (.-villagename mt)]
                   ;; [:td (.-tehsil mt)]
                   [:td (.-year mt)]
                   [:td (.-racknumber mt)]
                   [:td (.-description mt)]
                   ]))]]]
       [:div.col-sm-6.col-md-offset-5 [shared-state 0]]]]])


(defroute khasragirdwani-list "/khasragirdwani" []
  (let [onres (fn [json]
                (cond (= (get-status json) 200) (set-authorized-list json :khasragirdwanis render-khasragirdwani)
                      :else (sign-out)))]
    (set-url "/khasragirdwani")
    (set-key-value :is-searched-results false)
    (http-get-auth (str serverhost "khasragirdwanis?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


;; ---------------------------------------------------------
;; masavi-records


(defn masavi-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber [[v/required :message "Field is required Must be number"]]
                     :year [[v/required :message "Field is required Must be number"]
                            [year-after? :message "Year must greater than 1900"]
                            [year-before? :message "Year must less than 9999"]]
                     :racknumber [[v/required :message "Field is required"]]
                     :description [[v/required :message "Field is required"]])))

(defn masavi-input-int [id ttype data-set placeholder in-focus]
  [:input.form-control {:id id
                        :type ttype
                        :value (@data-set id)
                        :placeholder placeholder
                        :on-change #(swap! data-set assoc id (int (-> % .-target .-value )))
                        :on-blur  #(reset! in-focus "on")
                        }])


(defn masavi-input-int-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [masavi-input-int id ttype data-set label input-focus]]
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


(defn ms-sub-onchange [id val data-set]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (set-key-value :villages dt)))]
    (do (http-get-auth (str serverhost "subdivisions/" val  "/villages") res)
        (swap! data-set assoc id (js/parseInt val)))))

(defn ms-sub-sel-tag [id data data-set]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(kg-sub-onchange  id (-> % .-target .-value) data-set)}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-subdivisionname d)])])

(defn ms-form-sub-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [ms-sub-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])


(defn ms-villages-sel-tag [id data data-set ]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(swap! data-set assoc id (js/parseInt (-> % .-target .-value)))}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-villagename d)])])

(defn ms-form-villages-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [ms-villages-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])

(defn masavi-form-cancel [event]
  (accountant/navigate! "/masavi"))

(defn masavi-template [doc-name data-set focus save-function]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [masavi-input-int-row :serialnumber "Serial Number" "text" data-set focus]
       [ms-form-sub-sel "Sub Division Name" :subdivisionid (:subdivisions @storage) data-set]
       [ms-form-villages-sel "Village Name" :villageid (:villages @storage) data-set]
       [masavi-input-int-row :year "Year" "text" data-set focus]
       [masavi-input-row :racknumber "Rack Number" "text" data-set focus]
       [masavi-input-row :description "Description" "text" data-set focus]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click masavi-form-cancel } "Cancel"]]]
        ]]]]]])



(defn masavi-add-form-onclick [data-set focus]
  (if (= nil (revenue-form-validator @data-set))
    (let [onres (fn[json] (accountant/navigate! "/masavi"))]
      (http-post (str serverhost "masavis") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on"))


(defn masavi-update-form-onclick [data-set focus]
  (if (= nil (masavi-form-validator @data-set))
    (let [onres (fn[data] (accountant/navigate! "/masavi"))]
      (http-put (str serverhost "masavis/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on"))



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
           "Khasra Girdwani Add Form"
           add-data focus
           #(masavi-add-form-onclick add-data focus)])))

(defn masavi-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :serialnumber (.-serialnumber dmt)
                             :subdivisionid (.-subdivisionid dmt)
                             :villageid (.-villageid dmt)
                             :year (.-year dmt)
                             :racknumber (.-racknumber dmt)
                             :description (.-description dmt)
                             :isactive true})
        focus (r/atom nil)]
    (fn [] [masavi-template
           "masavi Update Form"
           update-data focus
           #(masavi-update-form-onclick update-data focus)])))


(defn masavi-update[id]
  (accountant/navigate! (str "/masavi/update/" id)))

(defn masavi-delete[id]
  (let [onres (fn [json]
                (accountant/navigate! "/masavi"))]
    (http-delete (str serverhost "masavis/" id)  onres)))


(defroute masavi-add-path "/masavi/add" []
  (if (nil? (get-value! :user)) (accountant/navigate! "/login")
      (let [onres (fn[json](
                           (set-key-value :subdivisions (getdata json))
                           (set-page! [masavi-add-template])))]
        (http-get-auth (str serverhost "subdivisions") onres))))

(defroute masavi-upd-path "/masavi/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [masavi-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :masavis)))])))
        sub-res (fn[json](set-key-value :subdivisions (getdata json)))]
    (do
      (http-get-auth (str serverhost "villages") onres)
      (http-get-auth (str serverhost "subdivisions") sub-res))))

(defn masavi-add [event]
  (accountant/navigate! "/masavi/add"))



(defn render-masavi [masavis]
  [:div.col-md-12
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3.box-title "List of masavi"]]
    [:div.box-body
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click masavi-add}]]
      [:div.table-responsive
       [:table {:class "table table-bordered table-striped dataTable"}
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
         (doall (for [mt masavis]
                  ^{:key (.-id mt)}
                  [:tr
                   (when (is-admin-or-super-admin)
                     [:td [button {:bs-style "success"
                                   :on-click  #(masavi-update(.-id mt))} "Update"]])
                   (when (is-admin-or-super-admin)
                     [:td  [button {:bs-style "danger"
                                    :on-click #(masavi-delete(.-id mt))} "Delete"]])
                   [:td (.-serialnumber mt)]
                   [:td (.-subdivisionname mt)]
                   [:td (.-villagename mt)]
                   ;; [:td (.-tehsil mt)]
                   [:td (.-year mt)]
                   [:td (.-racknumber mt)]
                   [:td (.-description mt)]
                   ]))]]]
       [:div.col-sm-6.col-md-offset-5 [shared-state 0]]]]])


(defroute masavi-list "/masavi" []
  (let [onres (fn [json]
                (cond (= (get-status json) 200) (set-authorized-list json :masavis render-masavi)
                      :else (sign-out)))]
    (set-url "/masavi")
    (set-key-value :is-searched-results false)
    (http-get-auth (str serverhost "masavis?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))

;; ---------------------------------------------------------
;; Consolidation-records


(defn consolidation-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber [[v/required :message "Field is required Must be number"]]
                     :year [[v/required :message "Field is required Must be number"]
                            [year-after? :message "Year must greater than 1900"]
                            [year-before? :message "Year must less than 9999"]]
                     :racknumber [[v/required :message "Field is required"]]
                     :description [[v/required :message "Field is required"]])))

(defn consolidation-input-int [id ttype data-set placeholder in-focus]
  [:input.form-control {:id id
                        :type ttype
                        :value (@data-set id)
                        :placeholder placeholder
                        :on-change #(swap! data-set assoc id (int (-> % .-target .-value )))
                        :on-blur  #(reset! in-focus "on")
                        }])


(defn consolidation-input-int-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [consolidation-input-int id ttype data-set label input-focus]]
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


(defn cons-sub-onchange [id val data-set]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (set-key-value :villages dt)))]
    (do (http-get-auth (str serverhost "subdivisions/" val  "/villages") res)
        (swap! data-set assoc id (js/parseInt val)))))

(defn cons-sub-sel-tag [id data data-set]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(cons-sub-onchange  id (-> % .-target .-value) data-set)}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-subdivisionname d)])])

(defn cons-form-sub-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [cons-sub-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])


(defn cons-villages-sel-tag [id data data-set ]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(swap! data-set assoc id (js/parseInt (-> % .-target .-value)))}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-villagename d)])])

(defn cons-form-villages-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [cons-villages-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])

(defn consolidation-form-cancel [event]
  (accountant/navigate! "/consolidation"))

(defn consolidation-template [doc-name data-set focus save-function]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [consolidation-input-int-row :serialnumber "Serial Number" "text" data-set focus]
       [cons-form-sub-sel "Sub Division Name" :subdivisionid (:subdivisions @storage) data-set]
       [cons-form-villages-sel "Village Name" :villageid (:villages @storage) data-set]
       [consolidation-input-int-row :year "Year" "text" data-set focus]
       [consolidation-input-row :racknumber "Rack Number" "text" data-set focus]
       [consolidation-input-row :description "Description" "text" data-set focus]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click consolidation-form-cancel } "Cancel"]]]
        ]]]]]])



(defn consolidation-add-form-onclick [data-set focus]
  (if (= nil (revenue-form-validator @data-set))
    (let [onres (fn[json] (accountant/navigate! "/consolidation"))]
      (http-post (str serverhost "consolidations") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on"))


(defn consolidation-update-form-onclick [data-set focus]
  (if (= nil (consolidation-form-validator @data-set))
    (let [onres (fn[data] (accountant/navigate! "/consolidation"))]
      (http-put (str serverhost "consolidations/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on"))



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
           "Khasra Girdwani Add Form"
           add-data focus
           #(consolidation-add-form-onclick add-data focus)])))

(defn consolidation-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :serialnumber (.-serialnumber dmt)
                             :subdivisionid (.-subdivisionid dmt)
                             :villageid (.-villageid dmt)
                             :year (.-year dmt)
                             :racknumber (.-racknumber dmt)
                             :description (.-description dmt)
                             :isactive true})
        focus (r/atom nil)]
    (fn [] [consolidation-template
           "consolidation Update Form"
           update-data focus
           #(consolidation-update-form-onclick update-data focus)])))


(defn consolidation-update[id]
  (accountant/navigate! (str "/consolidation/update/" id)))

(defn consolidation-delete[id]
  (let [onres (fn [json]
                (accountant/navigate! "/consolidation"))]
    (http-delete (str serverhost "consolidations/" id)  onres)))


(defroute consolidation-add-path "/consolidation/add" []
  (if (nil? (get-value! :user)) (accountant/navigate! "/login")
      (let [onres (fn[json](
                           (set-key-value :subdivisions (getdata json))
                           (set-page! [consolidation-add-template])))]
        (http-get-auth (str serverhost "subdivisions") onres))))

(defroute consolidation-upd-path "/consolidation/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [consolidation-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :consolidations)))])))
        sub-res (fn[json](set-key-value :subdivisions (getdata json)))]
    (do
      (http-get-auth (str serverhost "villages") onres)
      (http-get-auth (str serverhost "subdivisions") sub-res))))

(defn consolidation-add [event]
  (accountant/navigate! "/consolidation/add"))



(defn render-consolidation [consolidations]
  [:div.col-md-12
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3.box-title "List of consolidation"]]
    [:div.box-body
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click consolidation-add}]]
      [:div.table-responsive
       [:table {:class "table table-bordered table-striped dataTable"}
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
         (doall (for [mt consolidations]
                  ^{:key (.-id mt)}
                  [:tr
                   (when (is-admin-or-super-admin)
                     [:td [button {:bs-style "success"
                                   :on-click  #(consolidation-update(.-id mt))} "Update"]])
                   (when (is-admin-or-super-admin)
                     [:td  [button {:bs-style "danger"
                                    :on-click #(consolidation-delete(.-id mt))} "Delete"]])
                   [:td (.-serialnumber mt)]
                   [:td (.-subdivisionname mt)]
                   [:td (.-villagename mt)]
                   ;; [:td (.-tehsil mt)]
                   [:td (.-year mt)]
                   [:td (.-racknumber mt)]
                   [:td (.-description mt)]
                   ]))]]]
       [:div.col-sm-6.col-md-offset-5 [shared-state 0]]]]])


(defroute consolidation-list "/consolidation" []
  (let [onres (fn [json]
                (cond (= (get-status json) 200) (set-authorized-list json :consolidations render-consolidation)
                      :else (sign-out)))]
    (set-url "/consolidation")
    (set-key-value :is-searched-results false)
    (http-get-auth (str serverhost "consolidations?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


;; ---------------------------------------------------------
;; fieldbook-records


(defn fieldbook-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber [[v/required :message "Field is required Must be number"]]
                     :year [[v/required :message "Field is required Must be number"]
                            [year-after? :message "Year must greater than 1900"]
                            [year-before? :message "Year must less than 9999"]]
                     :racknumber [[v/required :message "Field is required"]]
                     :description [[v/required :message "Field is required"]])))

(defn fieldbook-input-int [id ttype data-set placeholder in-focus]
  [:input.form-control {:id id
                        :type ttype
                        :value (@data-set id)
                        :placeholder placeholder
                        :on-change #(swap! data-set assoc id (int (-> % .-target .-value )))
                        :on-blur  #(reset! in-focus "on")
                        }])


(defn fieldbook-input-int-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [fieldbook-input-int id ttype data-set label input-focus]]
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


(defn fb-sub-onchange [id val data-set]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (set-key-value :villages dt)))]
    (do (http-get-auth (str serverhost "subdivisions/" val  "/villages") res)
        (swap! data-set assoc id (js/parseInt val)))))

(defn fb-sub-sel-tag [id data data-set]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(fb-sub-onchange  id (-> % .-target .-value) data-set)}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-subdivisionname d)])])

(defn fb-form-sub-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [fb-sub-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])


(defn fb-villages-sel-tag [id data data-set ]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(swap! data-set assoc id (js/parseInt (-> % .-target .-value)))}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-villagename d)])])

(defn fb-form-villages-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [fb-villages-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])

(defn fieldbook-form-cancel [event]
  (accountant/navigate! "/fieldbook"))

(defn fieldbook-template [doc-name data-set focus save-function]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [fieldbook-input-int-row :serialnumber "Serial Number" "text" data-set focus]
       [fb-form-sub-sel "Sub Division Name" :subdivisionid (:subdivisions @storage) data-set]
       [fb-form-villages-sel "Village Name" :villageid (:villages @storage) data-set]
       [fieldbook-input-int-row :year "Year" "text" data-set focus]
       [fieldbook-input-row :racknumber "Rack Number" "text" data-set focus]
       [fieldbook-input-row :description "Description" "text" data-set focus]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click fieldbook-form-cancel } "Cancel"]]]
        ]]]]]])



(defn fieldbook-add-form-onclick [data-set focus]
  (if (= nil (revenue-form-validator @data-set))
    (let [onres (fn[json] (accountant/navigate! "/fieldbook"))]
      (http-post (str serverhost "fieldbooks") onres  (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on"))


(defn fieldbook-update-form-onclick [data-set focus]
  (if (= nil (fieldbook-form-validator @data-set))
    (let [onres (fn[data] (accountant/navigate! "/fieldbook"))]
      (http-put (str serverhost "fieldbooks/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set)))))
  (reset! focus "on"))



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
           "Khasra Girdwani Add Form"
           add-data focus
           #(fieldbook-add-form-onclick add-data focus)])))

(defn fieldbook-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :serialnumber (.-serialnumber dmt)
                             :subdivisionid (.-subdivisionid dmt)
                             :villageid (.-villageid dmt)
                             :year (.-year dmt)
                             :racknumber (.-racknumber dmt)
                             :description (.-description dmt)
                             :isactive true})
        focus (r/atom nil)]
    (fn [] [fieldbook-template
           "fieldbook Update Form"
           update-data focus
           #(fieldbook-update-form-onclick update-data focus)])))


(defn fieldbook-update[id]
  (accountant/navigate! (str "/fieldbook/update/" id)))

(defn fieldbook-delete[id]
  (let [onres (fn [json]
                (accountant/navigate! "/fieldbook"))]
    (http-delete (str serverhost "fieldbooks/" id)  onres)))


(defroute fieldbook-add-path "/fieldbook/add" []
  (if (nil? (get-value! :user)) (accountant/navigate! "/login")
      (let [onres (fn[json](
                           (set-key-value :subdivisions (getdata json))
                           (set-page! [fieldbook-add-template])))]
        (http-get-auth (str serverhost "subdivisions") onres))))

(defroute fieldbook-upd-path "/fieldbook/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [fieldbook-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :fieldbooks)))])))
        sub-res (fn[json](set-key-value :subdivisions (getdata json)))]
    (do
      (http-get-auth (str serverhost "villages") onres)
      (http-get-auth (str serverhost "subdivisions") sub-res))))

(defn fieldbook-add [event]
  (accountant/navigate! "/fieldbook/add"))



(defn render-fieldbook [fieldbooks]
  [:div.col-md-12
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3.box-title "List of fieldbook"]]
    [:div.box-body
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click fieldbook-add}]]
      [:div.table-responsive
       [:table {:class "table table-bordered table-striped dataTable"}
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
         (doall (for [mt fieldbooks]
                  ^{:key (.-id mt)}
                  [:tr
                   (when (is-admin-or-super-admin)
                     [:td [button {:bs-style "success"
                                   :on-click  #(fieldbook-update(.-id mt))} "Update"]])
                   (when (is-admin-or-super-admin)
                     [:td  [button {:bs-style "danger"
                                    :on-click #(fieldbook-delete(.-id mt))} "Delete"]])
                   [:td (.-serialnumber mt)]
                   [:td (.-subdivisionname mt)]
                   [:td (.-villagename mt)]
                   ;; [:td (.-tehsil mt)]
                   [:td (.-year mt)]
                   [:td (.-racknumber mt)]
                   [:td (.-description mt)]
                   ]))]]]
       [:div.col-sm-6.col-md-offset-5 [shared-state 0]]]]])


(defroute fieldbook-list "/fieldbook" []
  (let [onres (fn [json]
                (cond (= (get-status json) 200) (set-authorized-list json :fieldbooks render-fieldbook)
                      :else (sign-out)))]
    (set-url "/fieldbook")
    (set-key-value :is-searched-results false)
    (http-get-auth (str serverhost "fieldbooks?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))

;; ---------------------------------------------------------
;; misc records

(defn misc-form-validator [data-set]
  (first (b/validate data-set
                     :filenumber [[v/required :message "Field is required"]]
                     :subject [[v/required :message "Field is required"]]
                     :title [[v/required :message "Field is required"]]
                     :remarks [[v/required :message "Field is required"]]
                     :dispatcheddate [[v/required :message "Field is required"]
                                      [date-year-after? :message "Year must greater than 1900"]
                                      [date-year-before? :message "Year must less than 9999"]])))

(defn misc-receiveddate-validator [data-set]
  (first (b/validate data-set
                     :receiveddate [[v/required :message "Field is required"]
                                    [date-year-after? :message "Year must greater than 1900"]
                                    [date-year-before? :message "Year must less than 9999"]])))

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

(defn misc-reciveddate [id label ttype data-set focus bool]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [misc-input-element id ttype data-set label input-focus bool]]
       [:div.col-sm-3 (if @focus
                        (if (= nil (misc-receiveddate-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((misc-receiveddate-validator @data-set) id)))]])
                        [:div])]])))

(defn misc-form-cancel [event]
  (accountant/navigate! "/misc"))

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
       [misc-input-row :title "Nature of Case" "text" data-set focus bool]
       [misc-input-row :remarks "Remarks" "text" data-set focus bool]
       [misc-input-row :dispatcheddate "Dispatched Date" "date" data-set focus bool]
       [misc-reciveddate :receiveddate "Recevied Date" "date" data-set focus false]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click misc-form-cancel } "Cancel"]]] ]]]]]])

(defn misc-add-form-onclick [data-set focus]
  (if (= nil (@data-set :receiveddate))
    (if (= nil (misc-form-validator @data-set))
      (let [onres (fn[json] (accountant/navigate! "/misc"))]
        (http-post (str serverhost "miscs") onres  (.serialize (Serializer.) (clj->js @data-set))))
      (reset! focus "on"))
    (if (= nil (misc-form-validator @data-set) (misc-receiveddate-validator @data-set))
      (let [onres (fn[json] (accountant/navigate! "/misc"))]
        (http-post (str serverhost "miscs") onres  (.serialize (Serializer.) (clj->js @data-set))))
      (reset! focus "on"))))


(defn misc-update-form-onclick [data-set focus]
  (if (= nil (@data-set :receiveddate))
    (if (= nil (misc-form-validator @data-set))
      (let [onres (fn[data] (accountant/navigate! "/misc"))]
        (http-put (str serverhost "miscs/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set))))
      (reset! focus "on"))
    (if (= nil (misc-form-validator @data-set) (misc-receiveddate-validator @data-set))
      (let [onres (fn[data] (accountant/navigate! "/misc"))]
        (http-put (str serverhost "miscs/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set))))
      (reset! focus "on"))))


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
  (accountant/navigate! "/misc/add"))

(defn misc-update[id]
  (accountant/navigate! (str "/misc/update/" id)))

(defn misc-delete[id]
  (let [onres (fn [json]
                (accountant/navigate! "/misc"))]
    (http-delete (str serverhost "miscs/" id)  onres)))




(defroute misc-add-path "/misc/add" []
  (if (nil? (get-value! :user)) (accountant/navigate! "/login")
      (let [onres (fn[json](
                           (set-key-value :villages (getdata json))
                           (set-page! [misc-add-template])))]
        (http-get-auth (str serverhost "villages") onres))))

(defroute misc-upd-path "/misc/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :villages (getdata json))
                       (set-page! [misc-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :miscs)))])))]
    (http-get-auth (str serverhost "villages") onres)))

(defn render-misc [miscs]
  [:div.col-md-12
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3.box-title "List of Misc Records"]]
    [:div.box-body
     (when (is-admin-or-super-admin)
       [:div.form-group
        [:input {:type "button" :value "Add"
                 :class "btn btn-primary" :on-click misc-add}]])
      [:div.table-responsive
       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          [:th ""]
          (when (is-admin-or-super-admin) [:th " "])
          [:th "Fille Number"]
          [:th "Subject"]
          [:th "Nature of Case"]
          [:th "Remarks"]
          [:th "Dispatched Date"]
          [:th "Received Date"]
           ]]
        [:tbody
         (doall (for [mt miscs]
                  ^{:key (.-id mt)}
                  [:tr
                   (when (and  (.-dispatcheddate mt) (.-receiveddate mt))
                     {:style {:background-color "#b5f9c0"}})
                   [:td [button {:bs-style "success" :on-click  #(misc-update(.-id mt))} "Update"]]
                   (when (is-admin-or-super-admin)
                     [:td  [button {:bs-style "danger" :on-click #(misc-delete(.-id mt))} "Delete"]])
                   [:td (.-filenumber mt)]
                   [:td (.-subject mt)]
                   [:td (.-title mt)]
                   [:td (.-remarks mt)]
                   [:td (.-dispatcheddate mt)]
                   [:td (.-receiveddate mt)]]))]]]
       [:div.col-sm-6.col-md-offset-5 [shared-state 0]]]]])

(defroute misc-list "/misc" []
  (let [onres (fn [json]
                (cond (= (get-status json) 200) (set-authorized-list json :miscs render-misc)
                      :else (sign-out)))]
    (set-key-value :is-searched-results false)
    (http-get-auth (str serverhost "miscs?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))

;; ---------------------------------------------------------
;; o2register-records

(defn o2register-form-validator [data-set]
  (first (b/validate data-set
                     :o2number [[v/required :message "Field is required"]]
                     :subdivisionname [[v/required :message "Field is required"]]
                     ;; :startingdate [[v/required :message "Field is required"]
                     ;;                [date-year-after? :message "Year must greater than 1900"]
                     ;;                [date-year-before? :message "Year must less than 9999"]]
                     ;; :endingdate [[v/required :message "Field is required"]
                     ;;              [date-year-after? :message "Year must greater than 1900"]
                     ;;              [date-year-before? :message "Year must less than 9999"]]
                     :racknumber [[v/required :message "Field is required"]]
                     ;; :description [[v/required :message "Field is required"]]
                     )))

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

(defn o2register-form-cancel [event]
  (accountant/navigate! "/o2register"))

(defn o2num-sel-tag [id data data-set ]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(swap! data-set assoc id  (-> % .-target .-value))}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key  d}
     [:option {:value d} d])])

(defn o2-form-o2num-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [o2num-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])



(defn o2-subdiv-sel-tag [id data data-set ]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(swap! data-set assoc id  (-> % .-target .-value))}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-subdivisionname d)} (.-subdivisionname d)])])

(defn o2-form-subdiv-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [o2-subdiv-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])

(defn o2register-template [doc-name data-set focus save-function]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [o2register-input-row :o2number "O2 Number" "text" data-set focus]
       [o2register-input-row :startingdate "Starting Date" "date" data-set focus]
       [o2register-input-row :endingdate "Ending Date" "date" data-set focus]
       [o2-form-subdiv-sel "Sub Division Name" :subdivisionname (:subdivisions @storage) data-set]
       [o2register-input-row :racknumber "Rack Number" "text" data-set focus]
       [o2register-input-row :description "Description" "text" data-set focus]
       ;; [:div [:spn (str @data-set)]]
       ]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click o2register-form-cancel } "Cancel"]]]
        ]]]]])


(defn o2register-add-form-onclick [data-set focus]
  (if (= nil (o2register-form-validator @data-set))
    (let [onres (fn[json] (accountant/navigate! "/o2register"))]
      (http-post (str serverhost "o2registers") onres  (.serialize (Serializer.) (clj->js @data-set))))
    (reset! focus "on")))


(defn o2register-update-form-onclick [data-set focus]
  (if (= nil (o2register-form-validator @data-set))
        (let [onres (fn[data] (accountant/navigate! "/o2register"))]
          (http-put (str serverhost "o2registers/" (:id @data-set)) onres (.serialize (Serializer.) (clj->js @data-set))))
    (reset! focus "on")))

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
                             :startingdate (.-startingdate dmt)
                             :endingdate (.-endingdate dmt)
                             :subdivisionname (.-subdivisionname dmt)
                             :racknumber (.-racknumber dmt)
                             :description (.-description dmt)
                             })
        focus (r/atom nil)]
    (fn [] [o2register-template
           "O2 Register Update Form"
           update-data focus
           #(o2register-update-form-onclick update-data focus)])))


(defn o2register-update[id]
  (accountant/navigate! (str "/o2register/update/" id)))

(defn o2register-delete[id]
  (let [onres (fn [json]
                (accountant/navigate! "/o2register"))]
    (http-delete (str serverhost "o2registers/" id)  onres)))


(defroute o2register-add-path "/o2register/add" []
  (if (nil? (get-value! :user))
    (accountant/navigate! "/login")
    (let [onres (fn[json]
                  ((set-key-value :subdivisions (getdata json))
                   (set-page! [o2register-add-template])))
            ono2resp (fn [json]
                       (set-key-value :o2mutations (getdata json)))]

      (http-get-auth (str serverhost "mutations/o2numbers/search") ono2resp)
      (http-get-auth (str serverhost "subdivisions") onres))))

(defroute o2register-upd-path "/o2register/update/:id" [id]
  (let [onres (fn[json](
                       (set-key-value :subdivisions (getdata json))
                       (set-page! [o2register-update-template id
                                   (first (filter (fn[obj]
                                                    (=(.-id obj) (.parseInt js/window id))) (get-value! :o2registers)))])))
        ono2resp (fn [json] (set-key-value :o2mutations
                                          (clj->js (cons "select" (js->clj (getdata json))))))]
    (http-get-auth (str serverhost "mutations/o2numbers/search") ono2resp)
    (http-get-auth (str serverhost "subdivisions") onres)))

(defn o2register-add [event]
  (accountant/navigate! "/o2register/add"))

(defn render-o2register [o2registers]
  [:div.col-md-12
   [:div.box
    [:div.box-header
     [:h3.box-title "List of O2 Register Records"]]
      [:div.box-body
       [:div.form-group
        [:input {:type "button" :value "Add"
                 :class "btn btn-primary" :on-click o2register-add}]]
       [:div.table-responsive
        [:table {:class "table table-bordered table-striped dataTable"}
         [:thead
          [:tr
           (when (is-admin-or-super-admin) [:th " "])
           (when (is-admin-or-super-admin) [:th " "])
           [:th "O2 Number"]
           [:th "Starting Date"]
           [:th "Ending Date"]
           [:th "Sub Division Name"]
           [:th "Rack Number"]
           [:th "Description"]
           ]]
         [:tbody
          (doall (for [mt o2registers]
                   ^{:key (.-id mt)}
                   [:tr
                    (when (is-admin-or-super-admin)
                      [:td [button {:bs-style "success"
                                    :on-click  #(o2register-update(.-id mt))} "Update"]])
                    (when (is-admin-or-super-admin)
                      [:td  [button {:bs-style "danger"
                                     :on-click #(o2register-delete(.-id mt))} "Delete"]])
                    [:td (.-o2number mt)]
                    [:td (.-startingdate mt)]
                    [:td (.-endingdate mt)]
                    [:td (.-subdivisionname mt)]
                    [:td (.-racknumber mt)]
                    [:td (.-description mt)]
                    ]))]]]
       [:div.col-sm-6.col-md-offset-5  [shared-state 0]]]]])

(defroute o2register-list "/o2register" []
  (let [onres (fn [json]
                (cond (= (get-status json) 200) (set-authorized-list json :o2registers render-o2register)
                      :else (sign-out)))]
    (set-key-value :is-searched-results false)
    (http-get-auth (str serverhost "o2registers?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))

;; ---------------------------------------------------------
;; o4register-records

(defn o4register-form-validator [data-set]
  (first (b/validate data-set
                     :serialnumber  [[v/required :message "Field is required"]]
                     :subdivisionid [[v/required :message "Field is required"]]
                     :o4number [[v/required :message "Field is required"]]
                     :villageid [[v/required :message "Field is required"]]
                     :racknumber [[v/required :message "Field is required"]]
                     ;; :description [[v/required :message "Field is required"]]
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

(defn o4register-input-int-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-int id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (o4register-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((o4register-form-validator @data-set) id)))]])
                        [:div])]])))

(defn o4register-form-cancel [event]
  (accountant/navigate! "/o4register"))



(defn o4num-sel-tag [id data data-set ]
  [:input.form-control {:id id
                        :value (@data-set id)
                        :on-change #()}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key  (.-id d)}
     [:option {:value (.-o4number d)} (.-o4number d)])])

(defn o4-form-o4num-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [o4num-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])

(defn o4-sub-onchange [id val data-set]
  (let [res (fn [json]
              (let [dt (getdata json)]
                (set-key-value :villages dt)))]
    (do (http-get-auth (str serverhost "subdivisions/" val  "/villages") res)
        (swap! data-set assoc id (js/parseInt val)))))


(defn o4-sub-sel-tag [id data data-set]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(rev-sub-onchange  id (-> % .-target .-value) data-set)}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-subdivisionname d)])])

(defn o4-form-sub-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [o4-sub-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])


(defn o4-villages-sel-tag [id data data-set ]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(swap! data-set assoc id (js/parseInt (-> % .-target .-value)))}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-id d)} (.-villagename d)])])

(defn o4-form-villages-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [o4-villages-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])


(defn o4register-template [doc-name data-set focus save-function]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [o4register-input-int-row :serialnumber "Serial Number" "text" data-set focus ]
       [o4register-input-row :o4number "O4 Number" "text" data-set focus]
       [o4-form-sub-sel "Sub Division Name" :subdivisionid (:subdivisions @storage) data-set]
       [o4-form-villages-sel "Village Name" :villageid (:villages @storage) data-set]
       [o4register-input-row :racknumber "Rack Number" "text" data-set focus]
       [o4register-input-row :description "Description" "text" data-set focus]
       ]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click o4register-form-cancel } "Cancel"]]] ]]]]])


(defn o4register-add-form-onclick [data-set focus]
  (if (= nil (o4register-form-validator @data-set))
    (let [onres (fn[json] (accountant/navigate! "/o4register"))]
      (http-post (str serverhost "o4registers") onres  (.serialize (Serializer.) (clj->js @data-set))))
    (reset! focus "on")))

(defn o4register-update-form-onclick [data-set focus]
  (if (= nil (o4register-form-validator @data-set))
    (let [onres (fn[data] (accountant/navigate! "/o4register"))]
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
                             :serialnumber (.-serialnumber dmt)
                             :subdivisionid (.-subdivisionid dmt)
                             :villageid (.-villageid dmt)
                             :racknumber (.-racknumber dmt)
                             :description (.-description dmt)
                             })
        focus (r/atom nil)]
    (fn [] [o4register-template
           "O4 Register Update Form"
           update-data
           focus
           #(o4register-update-form-onclick update-data focus)])))


(defn o4register-add [event]
  (accountant/navigate! "/o4register/add"))

(defn o4register-update[id]
  (accountant/navigate! (str "/o4register/update/" id)))

(defn o4register-delete[id]
  (let [onres (fn [json]
                (accountant/navigate! "/o4register"))]
    (http-delete (str serverhost "o4registers/" id)  onres)))


(defroute o4register-add-path "/o4register/add" []
  (if (nil? (get-value! :user)) (accountant/navigate! "/login")
      (let [onres (fn[json](
                           (set-key-value :subdivisions (getdata json))
                           (set-page! [o4register-add-template])))
            ono4resp (fn [json] (set-key-value :o4mutations (getdata json)))]
        (http-get-auth (str serverhost "mutations/o4numbers/search") ono4resp)
        (http-get-auth (str serverhost "subdivisions") onres))))


(defroute o4register-upd-path "/o4register/update/:id" [id]
  (let [o4-res (fn [json] (set-key-value :o4mutations (getdata json)))
        vill-res (fn[json](set-key-value :villages (getdata json)))
        sub-res (fn[json](set-key-value :subdivisions (getdata json)))]
    (http-get-auth (str serverhost "mutations/o4numbers/search") o4-res)
    (http-get-auth (str serverhost "villages") vill-res)
    (http-get-auth (str serverhost "subdivisions") sub-res)
    (set-page! [o4register-update-template id
                (first (filter (fn[obj]
                                 (=(.-id obj) (.parseInt js/window id))) (get-value! :o4registers)))])))



(defn render-o4register [o4registers]
  [:div.col-md-12
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3.box-title "List of O4 Register Records"]]
    [:div.box-body
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click o4register-add}]]
      [:div.table-responsive
       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          (when (is-admin-or-super-admin) [:th " "])
          (when (is-admin-or-super-admin) [:th " "])
          [:th "Serial Number"]
          [:th "O4 Number"]
          [:th "Sub Division Name"]
          [:th "Village Name"]
          [:th "Rack Number"]
          [:th "Description"]
          ]]
        [:tbody
         (doall (for [mt o4registers]
                  ^{:key (.-id mt)}
                  [:tr
                   (when (is-admin-or-super-admin)
                     [:td [button {:bs-style "success"
                                   :on-click  #(o4register-update(.-id mt))} "Update"]])
                   (when (is-admin-or-super-admin)
                     [:td  [button {:bs-style "danger"
                                    :on-click #(o4register-delete(.-id mt))} "Delete"]])
                   [:td (.-serialnumber mt)]
                   [:td (.-o4number mt)]
                   [:td (.-subdivisionname mt)]
                   [:td (.-villagename mt)]
                   [:td (.-racknumber mt)]
                   [:td (.-description mt)]
                   ]))]]]
       [:div.col-sm-6.col-md-offset-5 [shared-state 0]]]]])

(defroute o4register-list "/o4register" []
  (let [onres (fn [json]
                (cond (= (get-status json) 200) (set-authorized-list json :o4registers render-o4register)
                      :else (sign-out)))]
    (set-key-value :is-searched-results false)
    (http-get-auth (str serverhost "o4registers?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))


;; ---------------------------------------------------------
;; o6register-records

(defn o6register-form-validator [data-set]
  (first (b/validate data-set
                     :subdivisionname [[v/required :message "Field is required"]]
                     :year [[v/required :message "Field is required"]
                            [year-after? :message "Year must greater than 1900"]
                            [year-before? :message "Year must less than 9999"]]
                     :misalnumber [[v/required :message "Field is required"]]
                     ;; :startingdate [[v/required :message "Field is required"]
                     ;;                [date-year-after? :message "Year must greater than 1900"]
                     ;;                [date-year-before? :message "Year must less than 9999"]]
                     ;; :endingdate [[v/required :message "Field is required"]
                     ;;              [date-year-after? :message "Year must greater than 1900"]
                     ;;              [date-year-before? :message "Year must less than 9999"]]
                     :racknumber  [[v/required :message "Field is required"]]
                     ;;:description [[v/required :message "Field is required"]]
                     )))

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

(defn o6register-input-int-row [id label ttype data-set focus]
  (let [input-focus (r/atom nil)]
    (fn []
      [:div.form-group
       [:label.col-sm-3.control-label label]
       [:div.col-sm-6 [input-int id ttype data-set label input-focus]]
       [:div.col-sm-3 (if (or @input-focus @focus)
                        (if (= nil (o6register-form-validator @data-set))
                          [:div]
                          [:div {:style  {:color "red"}}
                           [:b (str (first ((o6register-form-validator @data-set) id)))]])
                        [:div])]])))


(defn o6register-form-cancel [event]
  (accountant/navigate! "/o6register"))

(defn o6num-sel-tag [id data data-set ]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(swap! data-set assoc id  (-> % .-target .-value))}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key  (.-id d)}
     [:option {:value (.-o6number d)} (.-o6number d)])])


(defn o6-form-o6num-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [o6num-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])

(defn o6-subdiv-sel-tag [id data data-set ]
  [:select.form-control {:id id
                         :value (@data-set id)
                         :on-change #(swap! data-set assoc id  (-> % .-target .-value))}
   [:option {:value 0} "--Select--"]
   (for [d  data]
     ^{:key (.-id d) }
     [:option {:value (.-subdivisionname d)} (.-subdivisionname d)])])

(defn o6-form-subdiv-sel [label id opt-data data-set]
  [:div.form-group
   [:label.col-sm-3.control-label label]
   [:div.col-sm-6 [o6-subdiv-sel-tag id opt-data data-set]]
   [:div.col-sm-3 [:div]]])


(defn o6register-template [doc-name data-set focus save-function]
  [:div.container
   [:div.col-md-12
    [:div.box.box-info
     [:div.box-header.with-border
      [:h2.box-title doc-name]]
     [:div.form-horizontal
      [:div.box-body
       [o6register-input-row :o6number  "O6 Number" "text" data-set focus]
       [o6register-input-int-row :year "Year" "text" data-set focus]
       [o6register-input-row :misalnumber "Misal Number" "text" data-set focus]
       [o6register-input-row :startingdate "Starting Date" "date" data-set focus]
       [o6register-input-row :endingdate "Ending Date" "date" data-set focus]
       [o6-form-subdiv-sel "Sub Division Name" :subdivisionname (:subdivisions @storage) data-set]
       [o6register-input-row :racknumber "Rack Number" "text" data-set focus]
       [o6register-input-row :description "Description" "text" data-set focus]
       ;; [:div [:spn (str @data-set)]]
       ]
       [:div.box-footer
        [:div.col-sm-8.col-md-offset-5
         [button-tool-bar
          [button {:bs-style "success" :on-click save-function} "Save"]
          [button {:bs-style "danger" :on-click o6register-form-cancel } "Cancel"]]]
        ]]]]])

(defn o6register-add-form-onclick [data-set focus]
  (if (= nil (o6register-form-validator @data-set))
      (let [onres (fn[json] (accountant/navigate! "/o6register"))]
        (http-post (str serverhost "o6registers")
                   onres  (.serialize (Serializer.) (clj->js @data-set))))
    (reset! focus "on")))

(defn o6register-update-form-onclick [data-set focus]
  (if (= nil (o6register-form-validator @data-set))
        (let [onres (fn[data] (accountant/navigate! "/o6register"))]
          (http-put (str serverhost "o6registers/" (:id @data-set))
                    onres (.serialize (Serializer.) (clj->js @data-set))))
    (reset! focus "on")))

(defn o6register-add-template []
  (let [add-data (r/atom {:isactive true})
        focus (r/atom nil)]
    (fn [] [o6register-template
           "O6 Register Add Form"
           add-data focus
           #(o6register-add-form-onclick add-data focus)])))

(defn o6register-update-template [id dmt]
  (let [update-data (r/atom {:id (int id)
                             :o6number (.-o6number dmt)
                             :subdivisionname (.-subdivisionname dmt)
                             :year (.-year dmt)
                             :misalnumber (.-misalnumber dmt)
                             :startingdate (.-startingdate dmt)
                             :endingdate (.-endingdate dmt)
                             :racknumber (.-racknumber dmt)
                             :description (.-description dmt)
                             })
        focus (r/atom nil)]
    (fn [] [o6register-template
           "O6 Register Update Form"
           update-data focus
           #(o6register-update-form-onclick update-data focus)])))


(defn o6register-update[id]
  (accountant/navigate! (str "/o6register/update/" id)))

(defn o6register-delete[id]
  (let [onres (fn [json]
                (secretary/dispatch! "/o6register"))]
    (http-delete (str serverhost "o6registers/" id)  onres)))

(defroute o6register-add-path "/o6register/add" []
  (if (nil? (get-value! :user)) (accountant/navigate! "/login")
      (let [onres (fn[json](
                           (set-key-value :subdivisions (getdata json))
                           (set-page! [o6register-add-template])))
            ono6resp (fn [json] (set-key-value :o6mutations (getdata json)))]
        (http-get-auth (str serverhost "mutations/o6numbers/search") ono6resp)
        (http-get-auth (str serverhost "subdivisions") onres))))

(defroute o6register-upd-path "/o6register/update/:id" [id]
  (let [onres (fn[json]
                ((set-key-value :subdivisions (getdata json))
                 (set-page! [o6register-update-template id
                             (first (filter
                                     (fn[obj]
                                       (=(.-id obj) (.parseInt js/window id)))
                                     (get-value! :o6registers)))])))
        ono6resp (fn [json] (set-key-value :o6mutations (getdata json)))]
    (http-get-auth (str serverhost "mutations/o6numbers/search") ono6resp)
    (http-get-auth (str serverhost "subdivisions") onres)))

(defn o6register-add [event]
  (accountant/navigate! "/o6register/add"))

(defn render-o6register [o6registers]
  [:div.col-md-12
   [:div {:class "box"}
    [:div {:class "box-header"}
     [:h3.box-title "List of O6 Register Records"]]
    [:div.box-body
      [:div.form-group
       [:input {:type "button" :value "Add"
                :class "btn btn-primary" :on-click o6register-add}]]
      [:div.table-responsive
       [:table {:class "table table-bordered table-striped dataTable"}
        [:thead
         [:tr
          (when (is-admin-or-super-admin) [:th " "])
          (when (is-admin-or-super-admin) [:th " "])
          [:th "O6 Number"]
          [:th "Year"]
          [:th "Misal Number"]
          [:th "Starting Date"]
          [:th "Ending Date"]
          [:th "Sub Division Name"]
          [:th "Rack Number"]
          [:th "Description"]
          ]]
        [:tbody
         (doall (for [mt o6registers]
                  ^{:key (.-id mt)}
                  [:tr
                   (when (is-admin-or-super-admin)
                     [:td [button {:bs-style "success"
                                   :on-click  #(o6register-update(.-id mt))} "Update"]])
                   (when (is-admin-or-super-admin)
                     [:td  [button {:bs-style "danger"
                                    :on-click #(o6register-delete(.-id mt))}"Delete"]])
                   [:td (.-o6number mt)]
                   [:td (.-year mt)]
                   [:td (.-misalnumber mt)]
                   [:td (.-startingdate mt)]
                   [:td (.-endingdate mt)]
                   [:td (.-subdivisionname mt)]
                   [:td (.-racknumber mt)]
                   [:td (.-description mt)]
                   ]))]]]
       [:div.col-sm-6.col-md-offset-5 [shared-state 0]]]]])

(defroute o6register-list "/o6register" []
  (let [onres (fn [json]
                (cond (= (get-status json) 200) (set-authorized-list json :o6registers render-o6register)
                      :else (sign-out)))]
    (set-key-value :is-searched-results false)
    (http-get-auth (str serverhost "o6registers?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)))

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
                (cond (= (get-status json) 200) (set-authorized-list json :mutations render-mutations)
                      :else (sign-out)))
        dist-res (fn[json] (set-key-value :districts (getdata json)))]
    (do
      (set-key-value :is-searched-results false)
      (http-get-auth (str serverhost "mutations?pageIndex="(dec (get-value! :current-page))"&pageSize=10") onres)
      (http-get-auth (str serverhost "districts") dist-res))))

(defroute login-page "/login" []
  (if (nil? (get-value! :user))
    (do
      (set-key-value :page-location [login])
      (set-login-page))
    (accountant/navigate! "/")))

(defroute "*" []
  (js/alert "<h1>Not Found Page</h1>"))

(defn get-current-route! []
  (let [curl (get-item local-storage "session")]
    (cond (nil? curl) (do (set-login-page)
                          (str "/"))
          :else (do
                  ;(js/console.log (:user-info (reader/read-string (js->clj curl))))
                  (swap! storage assoc :user (:user (:user-info (reader/read-string (js->clj curl)))))
                  (swap! storage assoc :token  (:token (:user-info (reader/read-string (js->clj curl)))))
                  (reset-login-page)
                  (:current-url (reader/read-string (js->clj curl)))))))

(defn main
  []
                                        ; (secretary/set-config! :prefix "#")
                                        ;(secretary/dispatch! (get-current-route!))
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (secretary/dispatch! path))
    :path-exists?
    (fn [path]
      (secretary/locate-route path))})
  (accountant/dispatch-current!)

  (r/render [page]
            (.getElementById js/document "app1"))
  ;; (let [history (History.)]
  ;;   (events/listen history "navigate"
  ;;                  (fn [event]
  ;;                    (secretary/dispatch! (.-token event))))
  ;;   (.setEnabled history true))
  )

(defn nav! [token]
  (.setToken (History.) token))

(main)
