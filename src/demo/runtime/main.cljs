(ns demo.runtime.main
  (:require
   [zero.config :as zc]
   [zero.core :as z]
   [zero.extras.db :as db]
   [zero.extras.cdf :as cdf]
   [demo.shared]
   [zero.component]
   [zero.dom :as dom]))

(defonce !pending-request-resolvers (atom {}))
(defonce !ws (atom nil))
(defonce !id-seq (atom 0))

(defn handle-message [{:keys [id kind value]}]
  (case kind
    :action (value {})
    :response (when-let [{:keys [resolve reject]} (get @!pending-request-resolvers id)]
                (case value
                  :success (resolve nil)
                  :error (reject nil))
                (swap! !pending-request-resolvers dissoc id))))

(defn run-at-server! [action & {:keys [on-failure on-success]}]
  {:pre (z/act? action)}
  (let [id (swap! !id-seq inc)]
    (->
      (js/Promise.
        (fn [resolve reject]
          (swap! !pending-request-resolvers assoc id {:resolve resolve :reject reject})
          (.send ^js/WebSocket @!ws (cdf/write-str {:id id :kind :action :value action}))))
      (.then #(some-> on-success (apply nil)))
      (.catch #(some-> on-failure (apply nil))))
    nil))

(zc/reg-effects
  :at-server run-at-server!)

(defn main []
  (db/patch! [{:path [] :value (-> js/document (.getElementById "init-db") .-textContent cdf/read-str)}])
  (let [ws (js/WebSocket. (str "ws://" js/location.host "/socket"))]
    (dom/listen ::ws-message ws "message"
      (fn [^js/MessageEvent ev]
        (when (string? (.-data ev))
          (handle-message (cdf/read-str (.-data ev))))))
    (reset! !ws ws)))