(ns reagent-phonecat.core
  (:import [goog History])
  (:use [cljs.pprint :only [pprint]])
  (:require [ajax.core :as ajx]
            [reagent.core :as rg]
            [clojure.string :as str]
            [goog.events :as events]
            [bidi.bidi :as b :include-macros true]
            [goog.history.EventType :as EventType]
            ))

(enable-console-print!)

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
  .setToken history (nav-to-path routes nav))

(defn hook-browser-navigation!
  "Listen to navigation events and update the application state"
  [routes]
  (doto history
    (events/listen
      EventType/NAVIGATE
      (fn [event]
        (let [path (.-token event)
              nav (path-to-nav routes path)
              {:keys [page params]} nav]
          (js/console.log (str "Page: " page ", params: " params))
          (if page                                          ; If target page is known
            (reset! navigational-state nav)                 ; Then navigate to page
            (navigate-to! routes {:page :phones})))))       ; Else go to default page
    (.setEnabled true)))

; endregion

; region -- API --

(defn load-phone-details! [state phone-id]
  (ajx/GET (str "/phones/" phone-id ".json")
           :handler (fn [phone-data]
                      (swap! state assoc-in [:phone-by-id phone-id] phone-data))
           :error-handler (fn [error]
                            (.warn js/console
                                   (str "Failed to fetch phone data: " error)))
           :response-format :json
           :keywords? true))

(defn load-phones! "Fetch phones and update the state"
  [state]
  (ajx/GET "/phones/phones.json"
           {:handler         (fn [phones]
                               (swap! state assoc :phones phones))
            :error-handler   #(.warn js/console (str "Failed to fetch phones: " %))
            :response-format :json
            :keywords?       true})
  )

(defmulti load-page-data! (fn [page _] page))

(defmethod load-page-data! :phone-list
  [_ _] (load-phones! state))

(defmethod load-page-data! :phone-detail
  [_ {:keys [phone-id]}] (load-phone-details! state phone-id))

(defn watch-nav-changes! []
  (add-watch navigational-state
             ::watch-nav-changes
             (fn [_ _ old-state new-state]
               (when-not (= old-state new-state)
                 (let [{:keys [page params]} new-state]
                   (load-page-data! page params))))))

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
      <phone-list>
        <phone-item>
        ; ...
    <phone-detail-page>
      <phone-detail>
      <phone-spec>
        <phone-properties>
      checkmark)

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
        ^{:key (:name phone)} [<phone-item> phone]
        )]]))

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
      phone [<phone-detail> phone]
      :not-loaded-yet [:div "Phone data was not loaded yet"])))

(defn <phone-detail> [phone]
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
     [:img.phone {:src (first images)}]
     [:h1 name]
     [:p description]

     [:ul.phone-thumbs
      (for [img images]
        ^{:key img} [:li [:img {:src img}]])]

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
        [:dd additionalFeatures]]]]]))

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
  (load-phones! state)
  (hook-browser-navigation! routes)
  (watch-nav-changes!)
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
  (load-phone-details! state "motorola-xoom")
  (load-phone-details! state "motorola-atrix-4g")
  (pprint (:phone-by-id @state))
  (remove-watch navigational-state ::watch-nav-changes)

  (in-ns 'reagent-phonecat.core)
  )

; endregion