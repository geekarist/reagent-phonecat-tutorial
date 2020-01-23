(ns reagent-phonecat.core
  (:require [reagent.core :as rg]
            [clojure.string :as str])
  )

(enable-console-print!)

(defn say-hello! "Greets `name`, or the world if no name specified.
Try and call this function from the ClojureScript REPL."
  [& [name]]
  (print "Hello," (or name "World") "!"))

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

;; --------------------------------------------
;; Application data

(def hardcoded-phones-data [{:name        "Nexus S"
                             :description "Fast just got faster with Nexus S"}
                            {:name        "Motorola XOOM™ with Wi-Fi"
                             :description "The Next, Next Generation tablet."}])

;; --------------------------------------------
;; View components

(declare                                                    ;; here we declare our components to define their in an order that feels natural.
  <phones-list>
  <phone-item>)

(defonce state (rg/atom {:phones hardcoded-phones-data
                         :search ""}))

(defn <phones-list>
  "An unordered list of phones"
  [phones-list search]
  (let [phones-list-filtered (->> phones-list (filter #(matches-search? search %)))]
    [:div.container-fluid
     [:ul
      (for [phone phones-list-filtered]
        ^{:key (:name phone)} [<phone-item> phone]
        )]]))

(defn <phone-item> "An phone item component"
  [{:keys [name description] :as _}]
  [:li.phone-item
   [:span name]
   [:p description]])

(defn <search-component> "The search input box" [search]
  [:span "Search: "
   [:input {:type      "text"
            :value     search
            :on-change (fn [e] (swap! state update-search (-> e .-target .-value)))}]])

(defn <top-component> []
  (let [{:keys [phones search]} @state]
    [:div.container-fluid
     [:div.row
      [:div.col-md-2 [<search-component> search]]
      [:div.col-md-8 [<phones-list> phones search]]]]))

(defn update-search [state new-search]
  (assoc state :search new-search))

(defn mount-root "Creates the application view and injects ('mounts') it into the root element."
  []
  (rg/render
    [<top-component>]
    (.getElementById js/document "app")))

(defn init! []
  (mount-root))

(comment
  (in-ns 'reagent-phonecat.core)
  (matches-search? "yo2" {:yo "yo"})
  (if (matches-search? "yx.*" {:yo "yo" :yo2 "yo2"}) "OK" "Aww")
  (matches-search? "todo" {:name "todo" :description "todo"})
  (<phones-list> [{:name "Nexus S" :description "Fast just got faster with Nexus S"}] "faster")
  (<phone-item> {:name "Nexus S" :description "Fast just got faster with Nexus S"})
  (swap! state assoc :search "fast")
  (swap! state assoc :search "xoom")
  )