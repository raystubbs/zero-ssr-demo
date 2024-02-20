(ns zero.demo.server.main
  (:require
    [zero.core :refer [act bnd <<] :as z]
    [zero.extras.util :refer [<<ctx <<act]]
    [zero.html :as zh]
    [zero.config :as zc]
    [zero.extras.db :as-alias db]
    [zero.extras.dom :as-alias zd]
    [cognitect.transit :as t]
    [manifold.stream :as s]
    [aleph.http :as http]
    [clojure.java.io :as io])
  (:import
    (java.io ByteArrayInputStream ByteArrayOutputStream InputStream)
    (zero.impl.actions Action)
    (zero.impl.bindings Binding)
    (zero.impl.injection Injection)))

(defonce !posts (atom []))
(defonce !sockets (atom #{}))
(defonce !next-stream-id (atom 0))

(def transit-writers
  (t/write-handler-map
    {Action (t/write-handler (constantly "act")
              (fn [^Action a]
                (t/tagged-value "map" {:props (.-props a) :effects (.-effects a)})))
     Binding (t/write-handler (constantly "bnd")
               (fn [^Binding b]
                 (t/tagged-value "map" {:key (.-stream-key b) :props (.-props b) :args (.-args b)})))
     Injection (t/write-handler (constantly "inj")
                 (fn [^Injection i]
                   (t/tagged-value "map" {:key (.-injector-key i) :args (.-args i)})))}))

(defn ->transit [x]
  (let [out (ByteArrayOutputStream.)]
    (t/write (t/writer out :json {:handlers transit-writers}) x)
    (String. ^"[B" (.toByteArray out))))

(def transit-readers
  (t/read-handler-map
    {"act" (t/read-handler #(apply z/act (:props %) (:effects %)))
     "bnd" (t/read-handler #(apply z/bnd (:props %) (:key %) (:args %)))
     "inj" (t/read-handler #(apply z/<< (:key %) (:args %)))}))

(defn <-transit [x]
  (cond
    (instance? String x)
    (t/read (t/reader (ByteArrayInputStream. (.getBytes x)) :json {:handlers transit-readers}))

    (instance? InputStream x)
    (t/read (t/reader x :json {:handlers transit-readers}))))

(defn render-post [post]
  [:section
   ::z/style {:border-radius "0.25rem"
              :border "solid 1px gray"
              :padding "0.5rem"
              :margin-top "0.5rem"
              :width "25rem"
              :box-sizing :border-box}
   [:h2 (:title post)]
   [:p (:content post)]])

(defn run-at-clients! [action]
  {:pre [(instance? Action action)]}
  (doseq [s @!sockets]
    (s/put! s (->transit {:kind :action :value action}))))

(defn create-post! [post]
  (swap! !posts conj post)
  (run-at-clients! (act [::db/patch [{:path [:posts-markup] :conj (render-post post) :fnil [:div]}]])))

(zc/reg-effects
  :server/create-post create-post!
  :server/at-clients run-at-clients!)

(defn attr-writer [v _ _]
  (->transit v))

(zc/reg-attr-writers :demo/* attr-writer ::zd/* attr-writer)

(defn page
  [_]
  {:status 200
   :body (zh/html
           [:html
            [:head
             [:title "Zero SSR Demo"]
             [:script#init-db :type "application/transit+json"
              (->transit {:posts-markup [:div (map render-post @!posts)]})]
             [:script :src "/js/runtime.js"]]
            [:body
             [:div
              ::z/style {:display :grid
                         :grid-template-columns "1fr"
                         :grid-row-gap "0.5rem"
                         :width "25rem"}
              [::zd/echo
               :vdom [:input
                      :placeholder "Title"
                      :value (bnd ::db/path [:inputs :title])
                      ::z/on {:input (act [::db/patch [{:path [:inputs :title] :value (<<ctx ::z/event.data)}]])}]]
              [::zd/echo
               :vdom [:textarea
                      :placeholder "Content"
                      :value (bnd ::db/path [:inputs :content])
                      ::z/on {:input (act [::db/patch [{:path [:inputs :content] :value (<<ctx ::z/event.data)}]])}]]

              [:button "Post"
               [::zd/listen
                :event "click"
                :action (act
                          [:at-server (<<act [:server/create-post (<< ::db/path [:inputs])])]
                          [::db/patch [{:path [:inputs] :value {}}]])]]]
             [::zd/echo :vdom-ref (bnd ::db/path [:posts-markup])]]])})

(defn handle-msg [s msg]
  (let [{:keys [id kind value] :as parsed} (<-transit msg)]
    (case kind
      :action (if (instance? Action value)
                (do
                  (value {})
                  (s/put! s (->transit {:id id :kind :response :value :success})))
                (s/put! s (->transit {:id id :kind :response :value :error})))
      (s/put! s (->transit {:id id :kind :response :value :error})))))

(defn socket [req]
  (if-let [s @(http/websocket-connection req)]
    (do
      (s/consume (partial handle-msg s) s)
      (swap! !sockets conj s)
      (s/on-closed s #(swap! !sockets disj s))
      nil)
    {:status 404}))

(defn handler [req]
  (case (:uri req)
    "/" (page req)
    "/socket" (socket req)
    (let [f (io/resource (str "public" (:uri req)))]
      {:status 200 :body (slurp f)})))

(defn -main
  [& _args]
  (http/start-server handler {:port 8080}))