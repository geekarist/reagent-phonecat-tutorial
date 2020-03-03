(ns reagent-phonecat.core
  (:import [goog History])
  (:use [cljs.pprint :only [pprint]])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]
    [reagent-phonecat.core :refer [<? go-safe]])
  (:require
    [ajax.core :as ajx]
    [reagent.core :as rg]
    [clojure.string :as str]
    [goog.events :as events]
    [bidi.bidi :as b :include-macros true]
    [goog.history.EventType :as EventType]
    [cljs.core.async :as a]))

(enable-console-print!)

; region -- Errors --

(defn accept-or-throw [v]
  (if (instance? js/Error v) (throw v) v))

; endregion

; region -- State --

(defonce state (rg/atom {:phones      []
                         :search      ""
                         :order-prop  :name
                         :phone-by-id {}
                         :navigation  {:page   :phone-list
                                       :params {}}}))

(def navigational-state (rg/cursor state [:navigation]))

(def order-prop-state (rg/cursor state [:order-prop]))

; endregion

; region -- Routing --

(defonce history (History.))

(def routes
  ["/phones" {""              :phone-list
              ["/" :phone-id] :phone-detail}])

(defn path-to-nav [routes path]
  (let [matched (b/match-route routes path)
        {:keys [handler route-params]} matched]
    {:page   handler
     :params route-params}))

(defn nav-to-path [routes nav]
  (let [{:keys [page params]} nav
        flat-params (->> params seq flatten)]
    (apply b/path-for routes page flat-params)))

(defn navigate-to! [routes nav]
  (.setToken history (nav-to-path routes nav)))

(def =path-changes=
  (a/chan (a/sliding-buffer 1)
          (comp (map (fn [event] (.-token event)))
                (dedupe))))

(defn hook-browser-navigation!
  "Listen to navigation events and update the application state"
  []
  (doto history
    (events/listen
      EventType/NAVIGATE
      (fn [event]
        (a/put! =path-changes= event)))
    (.setEnabled true)))

(declare load-page-data)

(defn back-to-path [history path]
  (do (.replaceToken history path)
      path))

(defn listen-to-path-changes! [routes]
  (go (loop [last-path "/phones"]
        (when-let [next-path (a/<! =path-changes=)]
          (let [next-path-nav (path-to-nav routes next-path)
                {:keys [page params] :as next-path-nav-vect} next-path-nav
                new-last-path (cond
                                (nil? page) (back-to-path history last-path)
                                :else (try (let [load-page-channel (load-page-data page params)
                                                 load-page-data-fn (a/<! load-page-channel)]
                                             (swap! state load-page-data-fn)
                                             (reset! navigational-state next-path-nav-vect)
                                             next-path)
                                           (catch js/Object err
                                             (back-to-path history last-path))))]
            (recur new-last-path))))))

; endregion

; region -- API --

(defn ajax-call [{:keys [method uri] :as opts}]
  (let [=resp= (a/chan)]
    (ajx/ajax-request (assoc opts
                        :handler (fn [[ok r :as _]]
                                   (a/put! =resp=
                                           (if ok r
                                                  (ex-info "AJAX Error" {:request opts :response r}))))))
    =resp=))

(def ajax-defaults
  {:format          (ajx/json-request-format)
   :response-format (ajx/json-response-format {:keywords? true})})

(defn fetch-phone-list []
  (ajax-call (assoc ajax-defaults
               :method :get
               :uri "/phones/phones.json")))

(defn fetch-phone-details [phone-id]
  (let [path (str "/phones/" phone-id ".json")
        request (assoc ajax-defaults :method :get :uri path)]
    (ajax-call request)))

(defmulti load-page-data (fn [page _] page))

(defmethod load-page-data :phone-list
  [_ _]
  (go-safe (let [phones (<? (fetch-phone-list))]
             #(assoc % :phones phones))))

(defmethod load-page-data :phone-detail
  [_ {:keys [phone-id]}]
  (go-safe
    (let [phone-details (<? (fetch-phone-details phone-id))]
      #(assoc-in % [:phone-by-id phone-id] phone-details))))

; endregion

; region -- Components --

(defn matches-search? "Determines if a phone item matches a text query."
  [search data]
  (let [qp (-> search
               (or "")
               str/lower-case
               re-pattern)]
    (->> (vals data)
         (filter string?)
         (map str/lower-case)
         (some #(re-find qp %))
         )))

(defn update-search [state new-search]
  (assoc state :search new-search))

(defn- find-phone-by-id [phones id]
  (->> phones
       (filter #(= (:id %) id))
       first))

; @formatter:off

(declare
  <top-component>
    <phone-list-page>
      <search-component>
      <order-prop-select>
      <phone-list>
        <phone-item>
        ; ...
    <phone-detail-page>
      <phone-detail>
      <phone-spec>
        <phone-properties>)

(declare checkmark)

; @formatter:on

(defn <order-prop-select> []
  [:select {:value     @order-prop-state
            :on-change #(reset! order-prop-state (-> % .-target .-value keyword))}
   [:option {:value :name} "Alphabetical"]
   [:option {:value :age} "Newest"]])

(defn <phone-list>
  "An unordered list of phones"
  [phones-list search order-prop]
  (let [phones-list-filtered (->> phones-list
                                  (filter #(matches-search? search %))
                                  (sort-by order-prop))]
    [:div.container-fluid
     [:ul
      (for [phone phones-list-filtered]
        ^{:key (:name phone)} [<phone-item> phone])]]))

(defn <phone-item> "An phone item component"
  [{:keys [name snippet id imageUrl] :as _}]
  (let [phone-page-href (str "#/phones/" id)]
    [:li.thumbnail
     [:a.thumb {:href phone-page-href} [:img {:src imageUrl}]]
     [:a {:href phone-page-href} name]
     [:p snippet]]))

(defn <search-component> "The search input box" [search]
  [:span "Search: "
   [:input {:type      "text"
            :value     search
            :on-change (fn [e] (swap! state update-search (-> e .-target .-value)))}]])

(defn <phone-list-page> []
  (let [{:keys [phones search]} @state]
    [:div.container-fluid
     [:div.row
      [:div.col-md-2
       [:p [<search-component> search]]
       [:p "Sort by:" [<order-prop-select>]]]
      [:div.col-md-8 [<phone-list> phones search @order-prop-state]]]]))

(defn <phone-detail-page> [phone-id]                        ; <phone-page> in tutorial
  (let [phone-cursor (rg/cursor state [:phone-by-id phone-id])
        phone @phone-cursor]
    (cond
      phone ^{:key phone-id} [<phone-detail> phone]
      :not-loaded-yet [:div "Phone data was not loaded yet"])))

(defn <phone-detail> [phone]
  (let [{:keys [images]} phone
        local-state (rg/atom {:main-image (first images)})]
    (fn [phone]
      (let [{:keys [images name description availability additionalFeatures storage battery
                    connectivity android sizeAndWeight display hardware camera]} phone
            {:keys [ram flash]} storage
            {:keys [type talkTime standbyTime]} battery
            {:keys [cell wifi bluetooth infrared gps]} connectivity
            {:keys [os ui]} android
            {:keys [dimensions weight]} sizeAndWeight
            {:keys [screenSize screenResolution touchScreen]} display
            {:keys [cpu usb audioJack fmRadio accelerometer physicalKeyboard]} hardware
            {:keys [primary features]} camera]
        [:div
         [:img.phone {:src (:main-image @local-state)}]
         [:h1 name]
         [:p description]

         [:div {:style {"margin-top"    "1em"
                        "margin-bottom" "1em"
                        "margin-left"   "auto"
                        "margin-right"  "auto"}}
          (for [img images]
            ^{:key img} [:img {:src      img
                               :width    "100px"
                               :on-click #(swap! local-state assoc :main-image img)}])]

         [:ul.specs
          [<phone-spec> "Availability" [(cons "Network" availability)]]
          [<phone-spec> "Battery" [["Type" type]
                                   ["Talk Time" talkTime]
                                   ["Standby Time (max)" standbyTime]]]
          [<phone-spec> "Storage and Memory" [["Storage" flash]
                                              ["Memory" ram]] >]
          [<phone-spec> "Connectivity" [["Bluetooth" bluetooth]
                                        ["Cellular" cell]
                                        ["GPS" (checkmark gps)]
                                        ["Infrared" (checkmark infrared)]
                                        ["Wi-Fi" wifi]]]
          [<phone-spec> "Android" [["OS" os]
                                   ["UI" (if (empty? ui) "None" ui)]]]
          [<phone-spec> "Size and Weight" [(cons "Dimensions" dimensions)
                                           ["Weight" weight]]]
          [<phone-spec> "Display" [["Resolution" screenResolution]
                                   ["Size" screenSize]
                                   ["Touch Screen" (checkmark touchScreen)]]]
          [<phone-spec> "Hardware" [["Accelerometer" (checkmark accelerometer)]
                                    ["Audio Jack" audioJack]
                                    ["CPU" cpu]
                                    ["FM Radio" (checkmark fmRadio)]
                                    ["Physical Keyboard" (checkmark physicalKeyboard)]
                                    ["USB" usb]]]
          [<phone-spec> "Camera" [(cons "Features" features)
                                  ["Primary" primary]]]
          [:li
           [:dl
            [:dt "Additional features"]
            [:dd additionalFeatures]]]]]))))

(defn checkmark [input]
  (if input \u2713 \u2718))

(defn <phone-spec> [title kvs]
  [:li
   [:span title]
   [<phone-properties> kvs]])

(defn <phone-properties> [kvs]
  [:dl (->> kvs
            (mapcat (fn [[t & ds]]
                      [^{:key t} [:dt t] (for [d ds] ^{:key d} [:dd d])])))])

(defn <top-component> []
  (let [{:keys [page params]} @navigational-state]
    [:div.container-fluid
     (case page
       :phone-list [<phone-list-page>]
       :phone-detail (let [phone-id (:phone-id params)]
                       [<phone-detail-page> phone-id])
       [:div "Sorry, the requested page does not exist"])]))

; endregion

; region -- Startup --

(defn mount-root "Creates the application view and injects ('mounts') it into the root element."
  []
  (rg/render
    [<top-component>]
    (.getElementById js/document "app")))

(defn init! []
  (hook-browser-navigation!)
  (listen-to-path-changes! routes)
  (mount-root))

; endregion

; region -- REPL --

(comment
  (matches-search? "yo2" {:yo "yo"})
  (if (matches-search? "yx.*" {:yo "yo" :yo2 "yo2"}) "OK" "Aww")
  (matches-search? "todo" {:name "todo" :description "todo"})
  (<phone-list> [{:name "Nexus S" :description "Fast just got faster with Nexus S"}] "faster")
  (<phone-item> {:name "Nexus S" :description "Fast just got faster with Nexus S"})
  (swap! state assoc :search "fast")
  (swap! state assoc :search "xoom")
  (let [my-atom (atom)]
    (load-phones! my-atom))
  (reset! navigational-state {:page :phone-list :params {}})
  (reset! navigational-state
          {:page   :phone-detail
           :params {:phone-id "motorola-xoom"}})
  (find-phone-by-id (:phones @state) "motorola-xoom")
  routes
  (b/match-route routes "/phones")
  (b/match-route routes "/phones/bla")
  (path-to-nav routes "/phones")
  (nav-to-path routes {:page :phone-list})
  (path-to-nav routes "/phones/moto")
  history
  (hook-browser-navigation! routes)
  (pprint (:phone-by-id @state))
  (remove-watch navigational-state ::watch-nav-changes)

  (go (-> (fetch-phone-list)
          (a/<!)
          (str)
          (prn)))

  (in-ns 'reagent-phonecat.core)
  )

; endregion