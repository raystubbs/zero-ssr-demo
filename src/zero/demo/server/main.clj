(ns zero.demo.server.main
  (:require
    [zero.core :refer [act bnd << <<ctx <<act] :as z]
    [zero.html :as zh]
    [zero.config :as zc]
    [zero.extras.db :as-alias db]
    [cognitect.transit :as t]
    [manifold.stream :as s]
    [aleph.http :as http]
    [clojure.java.io :as io])
  (:import
    (java.io ByteArrayInputStream ByteArrayOutputStream InputStream)))

(defonce !posts (atom []))
(defonce !sockets (atom #{}))

(def default-write-handler
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
        (z/inj? v) (z/inj->map v))))))

(defn ->transit [x]
  (let [out (ByteArrayOutputStream.)]
    (t/write (t/writer out :json {:default-handler default-write-handler}) x)
    (String. ^"[B" (.toByteArray out))))

(def transit-readers
  (t/read-handler-map
    {"act" (t/read-handler z/map->act)
     "bnd" (t/read-handler z/map->bnd)
     "inj" (t/read-handler z/map->inj)}))

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
  {:pre [(z/act? action)]}
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

(zc/reg-attr-writers ::z/* attr-writer)

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
              [:input
               :placeholder "Title"
               ::z/bind {:value (bnd ::db/path [:inputs :title])}
               ::z/on {:input (act [::db/patch [{:path [:inputs :title] :value (<<ctx ::z/event.data)}]])}]
              [:textarea
               :placeholder "Content"
               ::z/bind {:value (bnd ::db/path [:inputs :content])}
               ::z/on {:input (act [::db/patch [{:path [:inputs :content] :value (<<ctx ::z/event.data)}]])}]

              [:button
               ::z/on {:click (act
                               [:at-server (<<act [:server/create-post (<< ::db/path [:inputs])])]
                               [::db/patch [{:path [:inputs] :value {}}]])}
               "Post"]]
             [::z/echo
              ::z/bind {:vdom (bnd ::db/path [:posts-markup])}]]])})

(defn handle-msg [s msg]
  (let [{:keys [id kind value] :as parsed} (<-transit msg)]
    (case kind
      :action (if (z/act? value)
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