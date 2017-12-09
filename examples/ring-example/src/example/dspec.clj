(ns example.dspec
  (:require [reitit.ring.coercion-middleware :as coercion]
            [reitit.ring.coercion.spec :as spec-coercion]))

(def routes
  ["/dspec"
   ["/plus" {:name ::plus
             :coercion spec-coercion/coercion
             :responses {200 {:schema {:total int?}}}
             :get {:summary "plus with query-params"
                   :parameters {:query {:x int?, :y int?}}
                   :handler (fn [{{{:keys [x y]} :query} :parameters}]
                              {:status 200
                               :body {:total (+ x y)}})}
             :post {:summary "plus with body-params"
                    :parameters {:body {:x int?, :y int?}}
                    :handler (fn [{{{:keys [x y]} :body} :parameters}]
                               {:status 200
                                :body {:total (+ x y)}})}}]])