(ns demo.shared
  (:require
   [zero.extras.cdf :as cdf]
   [zero.config :as zc]
   [zero.dom :as-alias dom]
   [zero.core :as z]))

(defn attr-reader [s _ _] (cdf/read-str s))
(defn attr-writer [x _ _] (cdf/write-str x))

(zc/reg-attr-readers ::dom/* attr-reader ::* attr-reader)
(zc/reg-attr-writers ::dom/* attr-writer ::* attr-writer)

(zc/reg-components
  ::fancy-button
  {:focus :self
   :view (fn [_]
           [:root>
            ::z/style {:display "inline-flex"
                       :border-radius "0.25rem"
                       :border "1px solid gray"
                       :background "none"
                       :color "black"
                       :padding "0.25rem"
                       :justify-content "center"
                       :cursor "pointer"}
            [:slot]])})