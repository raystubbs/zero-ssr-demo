(ns zero.demo.runtime.main
  (:require
    [zero.config :as zc]
    [zero.core :as z]
    [zero.extras.dom :as zd]
    [zero.extras.db :as db]
    [cognitect.transit :as t]))

(def transit-reader
  (t/reader :json
    {:handlers
     {"act" z/map->act
      "bnd" z/map->bnd
      "inj" z/map->inj}}))

(def transit-writer
  (t/writer :json
    {:handlers
     {:default
      (t/write-handler
       (fn [v]
         (cond
           (z/act? v) "act"
           (z/bnd? v) "bnd"
           (z/inj? v) "inj"
           :else (throw (ex-info "can't serialize" {:v v}))))
       (fn [v]
         (t/tagged-value
          "map"
          (cond
            (z/act? v) (z/act->map v)
            (z/bnd? v) (z/bnd->map v)
            (z/inj? v) (z/inj->map v)))))}}))

(defn ->transit [x]
  (t/write transit-writer x))

(defn <-transit [s]
  (t/read transit-reader s))

(defn attr-reader [s _ _]
  (<-transit s))

(defn attr-writer [x _ _]
  (->transit x))

(zc/reg-attr-readers ::z/* attr-reader)
(zc/reg-attr-writers ::z/* attr-writer)

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