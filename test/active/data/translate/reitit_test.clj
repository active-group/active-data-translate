(ns active.data.translate.reitit-test
  (:require [active.data.translate.reitit :as sut]
            [active.data.translate.format :as format]
            [active.data.realm :as realm]
            [active.data.record :as r]
            [active.clojure.lens :as lens]
            [reitit.ring.coercion :as rrc]
            [reitit.ring :as ring]
            [clojure.test :as t]))

(r/def-record plus-request
  [req-x :- realm/integer
   req-y :- realm/integer])

(r/def-record plus-response
  [res-value :- realm/integer])

(def my-body-format
  (format/format :my-body-format
                 (format/combine-formatters
                  (format/simple-formatters {realm/integer lens/id})
                  (format/record-map-formatters
                   {plus-request [:x :y]
                    plus-response [:total]}))))

(def plus-endpoint
  {:coercion (sut/realm-coercion my-body-format)
   :parameters {:body plus-request
                :path {:bar realm/integer}
                :query {:foo realm/integer}}
   :responses {200 {:body plus-response}}
   :handler (fn [{:keys [parameters]}]
              (let [total (+ (-> parameters :body req-x)
                             (-> parameters :body req-y)
                             (-> parameters :path :bar)
                             (-> parameters :query :foo))]
                {:status 200
                 :body (plus-response {res-value total})}))})

(def app
  (ring/ring-handler
   (ring/router
    ["/api"
     ["/plus/:bar" {:name ::plus
                    :post plus-endpoint}]]
    {:data {:middleware [rrc/coerce-exceptions-middleware
                         rrc/coerce-request-middleware
                         rrc/coerce-response-middleware]}})))

(t/deftest valid-request
  (t/is (= {:status 200, :body {:total 11}}
           (app {:request-method :post
                 :uri "/api/plus/3"
                 :query-params {"foo" "5"}
                 :body-params {:x 1 :y 2}})))

  (t/testing "extra query param is ok"
    ;; see open-model; this is what reitit expects per default, I think.
    (t/is (= {:status 200 :body {:total 11}}
             (app {:request-method :post
                   :uri "/api/plus/3"
                   :query-params {"foo" "5" "baz" "1"}
                   :body-params {:x 1 :y 2}})))))

(t/deftest invalid-request
  (t/testing "invalid body param"
    (t/is (= 400
             (:status (app {:request-method :post
                            :uri "/api/plus/3"
                            :query-params {"foo" "5"}
                            :body-params {:y 2}})))))
  (t/testing "missing query param"
    (t/is (= 400
             (:status (app {:request-method :post
                            :uri "/api/plus/3"
                            :body-params {:x 1 :y 2}})))))

  (t/testing "invalid path param"
    (t/is (= 400
             (:status (app {:request-method :post
                            :uri "/api/plus/bla"
                            :body-params {:x 1 :y 2}}))))))
