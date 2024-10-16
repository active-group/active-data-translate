(ns active.data.translate.ajax
  (:require [ajax.core :as ajax]
            [active.data.translate.core :as core]))

(defn request-interceptor [format request-realm]
  (let [from (core/translator-from request-realm format)]
    (ajax/to-interceptor {:name "active-data realm translate request"
                          :request (fn [request]
                                     ;; TODO: :params or :body here? only for POST etc? resp. use strings-format for GET params
                                     (if (contains? request :params)
                                       (update request :params from)
                                       request))})))

(defn wrap-response-handler [format response-realm]
  (let [to (core/translator-to response-realm format)]
    (fn [handler]
      (when handler
        (comp handler to)))))

(defn- flip [f]
  (fn [& args]
    (apply f (reverse args))))

(defn use-format [request realm-format & {strings-format :strings}]
  ;; Note: if :format and :response-format are not set in the request,
  ;; the request format defaults to :transit, and the response format
  ;; defaults to an auto-detection; so the realm-format would have to
  ;; deal with potentially multiple formats. So better set it.

  ;; Note: response interceptors seem to be run before the reponse-format has been applied; i.e. on the raw data.
  ;; So to convert to realm after the transit parser, we have to wrap the handler instead.
  (cond-> request
    ;; TODO: convert query and path params according to strings-format
    ;; Note: not sure if putting it at the end or the beginning of the interceptors list is 'correct'
    (contains? request :realm) (update :interceptors (flip cons) (request-interceptor realm-format (:realm request)))
    (contains? request :response-realm) (update :handler (wrap-response-handler realm-format (:response-realm request)))))

(defn use-transit-format [request realm-format]
  (-> request
      (assoc :format :transit
             :response-format :transit)
      (use-format realm-format)))

;; TODO: also do something with path and query params? (in correspondence with reitit/coercer?
