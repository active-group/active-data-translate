(ns active.data.translate.ajax
  (:require [ajax.core :as ajax]
            [active.data.translate.core :as core]
            [active.data.translate.common :as common]))

(defn- request-body-interceptor [format request-realm]
  (let [from (core/translator-from request-realm format)]
    (ajax/to-interceptor {:name "active-data realm translate request"
                          :request (fn [request]
                                     ;; :body overrides :params
                                     ;; Note: to use a 'raw body' users should not set a :realm in the request.
                                     (if (contains? request :body)
                                       (update request :body from)
                                       (update request :params from)))})))

(defn- request-query-interceptor [format request-realms]
  (let [froms (->> request-realms
                   (map (fn [[key realm]]
                          [key (core/translator-from realm format)]))
                   (into {}))]
    (ajax/to-interceptor {:name "active-data realm translate query"
                          :request (fn [request]
                                     ;; :body overrides :params (use :realm instead of :params-realms to format that)
                                     (if (contains? request :body)
                                       request
                                       (update request :params
                                               (fn [m]
                                                 (reduce-kv (fn [res key from]
                                                              (assoc res key (from (get m key))))
                                                            {}
                                                            froms)))))})))

(defn- wrap-response-handler [format response-realm]
  (let [to (core/translator-to response-realm format)]
    (fn [handler]
      (when handler
        (comp handler to)))))

(defn- flip [f]
  (fn [& args]
    (apply f (reverse args))))

(defn prepare-use-format [request body-format & {string-format :strings}]
  (let [string-format (or string-format common/default-string-format)]
    (cond-> request
      ;; Note: not sure if putting it at the end or the beginning of the interceptors list is 'correct'
      (contains? request :realm) (update :interceptors (flip cons) (request-body-interceptor body-format (:realm request)))
      (contains? request :params-realms) (update :interceptors (flip cons) (request-query-interceptor string-format (:params-realms request)))
      true (dissoc :realm :params-realms))))

(defn use-format [request body-format & {string-format :strings}]
  ;; Note: if :format and :response-format are not set in the request,
  ;; the request format defaults to :transit, and the response format
  ;; defaults to an auto-detection; so the realm-format would have to
  ;; deal with potentially multiple formats. So better set it.

  ;; Note: response interceptors seem to be run before the
  ;; reponse-format has been applied; i.e. on the raw data.  So to
  ;; convert to realm after the transit parser, we have to wrap the
  ;; handler instead.

  ;; Note: unlike in reitit, path params are not represented
  ;; explicitly in ajax.core; can't easily convert them using
  ;; string-format.

  ;; Note: this can all be done before the actual :params/:body is
  ;; present in the request; that can be more efficient than doing
  ;; is for each actual request.

  ;; Note: we could split request and response body formats?
  (cond-> request
    ;; Note: prepare-use-format doesn't hurt to do again
    true (prepare-use-format body-format :strings string-format)
    (contains? request :response-realm) (update :handler (wrap-response-handler body-format (:response-realm request)))
    true (dissoc :response-realm)))

(defn prepare-use-transit-format [request body-format & opts]
  (as-> request $
    (assoc $
           :format :transit
           :response-format :transit)
    (apply prepare-use-format $ body-format opts)))

(defn use-transit-format [request body-format & opts]
  (as-> request $
    (apply prepare-use-transit-format $ body-format opts)
    (apply use-format $ body-format opts)))
