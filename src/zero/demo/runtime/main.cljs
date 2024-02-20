(ns zero.demo.runtime.main
  (:require
    [zero.config :as zc]
    [zero.core :refer [act bnd <<] :as z]
    [zero.extras.dom :as zd]
    [zero.extras.db :as db]
    [zero.extras.util :as zu]
    [cognitect.transit :as t]
    [zero.impl.actions :refer [Action]]
    [zero.impl.bindings :refer [Binding]]
    [zero.impl.injection :refer [Injection]]))

(def transit-reader
  (t/reader :json
    {:handlers
     {"act" (fn [{:keys [props effects]}] (apply act props effects))
      "bnd" (fn [{:keys [key props args]}] (apply bnd props key args))
      "inj" (fn [{:keys [key args]}] (apply << key args))}}))

(def transit-writer
  (t/writer :json
    {:handlers
     {Action (t/write-handler (constantly "act")
               (fn [^Action a]
                 (t/tagged-value "map" {:props (.-props a) :effects (.-effects a)})))
      Binding (t/write-handler (constantly "bnd")
                (fn [^Binding b]
                  (t/tagged-value "map" {:key (.-stream-key b) :props (.-props b) :args (.-args b)})))
      Injection (t/write-handler (constantly "inj")
                  (fn [^Injection i]
                    (t/tagged-value "map" {:key (.-injector-key i) :args (.-args i)})))}}))

(defn ->transit [x]
  (t/write transit-writer x))

(defn <-transit [s]
  (t/read transit-reader s))

(defn attr-reader [s _ _]
  (<-transit s))

(defn attr-writer [x _ _]
  (->transit x))

(zc/reg-attr-readers :demo/* attr-reader ::zd/* attr-reader)
(zc/reg-attr-writers :demo/* attr-writer ::zd/* attr-writer)

(defonce !pending-request-resolvers (atom {}))
(defonce !ws (atom nil))
(defonce !id-seq (atom 0))

(defn handle-message [{:keys [id kind value]}]
  (case kind
    :action (do (value {})
              (js/console.log "db: " (db/get [])))
    :response (when-let [{:keys [resolve reject]} (get @!pending-request-resolvers id)]
                (case value
                  :success (resolve nil)
                  :error (reject nil))
                (swap! !pending-request-resolvers dissoc id))))

(defn run-at-server! [action & {:keys [on-failure on-success]}]
  {:pre (instance? Action action)}
  (let [id (swap! !id-seq inc)]
    (->
      (js/Promise.
        (fn [resolve reject]
          (swap! !pending-request-resolvers assoc id {:resolve resolve :reject reject})
          (.send ^js/WebSocket @!ws (->transit {:id id :kind :action :value action}))))
      (.then #(some-> on-success (apply nil)))
      (.catch #(some-> on-failure (apply nil))))
    nil))

(zc/reg-effects
  :at-server run-at-server!)

(defn main []
  (db/patch! [{:path [] :value (-> js/document (.getElementById "init-db") .-textContent <-transit)}])
  (let [ws (js/WebSocket. (str "ws://" js/location.host "/socket"))]
    (zd/listen ws "message" ::ws-message
      (fn [^js/MessageEvent ev]
        (when (string? (.-data ev))
          (handle-message (<-transit (.-data ev))))))
    (reset! !ws ws)))