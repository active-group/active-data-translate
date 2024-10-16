(ns active.data.translate.ajax-test
  (:require [active.data.translate.ajax :as sut]
            [ajax.core :as ajax :include-macros true]
            [active.data.translate.shared-example :as ex]
            [clojure.test :as t]
            [cognitect.transit :as ctransit]
            [ajax.protocols]))

(defrecord InterceptXhrIo [info-atom response]
  ajax.protocols/AjaxImpl
  (-js-ajax-request [_this request handler]
    (reset! info-atom (select-keys request [:body :headers]))
    (handler (ajax.protocols/map->Response response))))

(defn intercept [request info-atom response]
  (assoc request :api (InterceptXhrIo. info-atom response)))

(defn with-interceptor
  "Returns the raw request that went out (after unparsing) and the result that came back (after formatting)."
  [f]
  (let [raw-request (atom nil)
        result (atom nil)]
    (f (fn [request raw-response]
         (-> request
             (assoc :handler (fn [res]
                               (reset! result res)))
             (intercept raw-request raw-response))))
    ;; only synchronous because of the Impl above.
    [(-> @raw-request
         (update :headers dissoc "Accept"))
     @result]))

(defn- to-transit [response]
  (-> response
      (update :body (fn [value]
                      ;; from clojure to transit str...
                      (ctransit/write (ctransit/writer :json) value)))
      (update :headers assoc "Content-Type" "application/transit")))

(defn- from-transit [request]
  (assert (= "application/transit+json"
             (get (:headers request) "Content-Type")))
  (let [r (-> request
              (update :headers dissoc "Content-Type")
              (update :body (fn [str]
                              ;; from transit str to clojure...
                              (ctransit/read (ctransit/reader :json) str))))]
    (if (empty? (:headers r))
      (dissoc r :headers)
      r)))

(defn with-transit-interceptor
  [f]
  (let [[raw-request result]
        (with-interceptor
          (fn [wrap-request]
            (f (fn [request response]
                 (-> request
                     (assoc :format :transit
                            :response-format :transit)
                     (wrap-request (to-transit response)))))))]
    [(from-transit raw-request)
     result]))

(t/deftest interceptor-test
  (let [[raw-request result]
        (with-interceptor
          (fn [wrap-request]
            (ajax/POST "/" (-> {:params {:x "x"}
                                :format :transit
                                :response-format :transit}
                               (wrap-request {:body "[\"^ \",\"~:y\",\"y\"]"
                                              :headers {"Content-Type" "application/transit+json"}
                                              :status 200})))))]
    (t/is (= {:y "y"} result))
    (t/is (= {:body "[\"^ \",\"~:x\",\"x\"]"
              :headers {"Content-Type" "application/transit+json"}}
             raw-request)))

  (let [[raw-request result]
        (with-transit-interceptor
          (fn [wrap-request]
            (ajax/POST "/" (-> {:params {:x "x"}}
                               (wrap-request {:body {:y "y"}
                                              :status 200})))))]
    (t/is (= {:y "y"} result))
    (t/is (= {:body {:x "x"}} raw-request))))

(t/deftest post-params-test
  (let [[raw-request result]
        (with-transit-interceptor
          (fn [wrap-request]
            (ajax/POST "/" (-> {:realm ex/plus-request
                                :response-realm ex/plus-response
                                :params (ex/plus-request {ex/req-x 1 ex/req-y 3})}
                               (wrap-request {:body {:total 4}
                                              :status 200})
                               (sut/use-transit-format ex/my-body-format)))))]
    (t/is (= {:body {:x 1, :y 3}} raw-request))
    ;; Note: if they are not=, the test runner might fail to calculate a diff (because of dissoc restriction in records)
    (t/is (= (ex/plus-response {ex/res-value 4}) result))))
