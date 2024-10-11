(ns active.data.translate.format
  (:require [active.data.realm.inspection :as realm-inspection])
  (:refer-clojure :exclude [format]))

;; Translators are lenses, resp.
;; functions of the form
;; (fn ([format-value] realm-value)
;;     ([_ realm-value] format-value))

;; A Format is an identifier together with default translators for
;; some realms.

;; Format

(defrecord ^:private Format [id get-default-translator])

(def format? (partial instance? Format))

(declare unsupported-exn)

(defn format
  ([id]
   (format id (fn [realm _recurse] (throw (unsupported-exn realm)))))
  ([id get-default-translator]
   (Format. id get-default-translator)))

(defn format-id [format]
  (assert (format? format))
  (:id format))

(defn unsupported-exn
  ;; culprit should be a realm, or a simple value.
  ([culprit]
   (unsupported-exn culprit nil))
  ([culprit realms-path]
   (unsupported-exn nil culprit realms-path))
  ([format culprit realms-path]
   (ex-info (if format
              ;; TODO: if culprit is not a realm, the message should be different; then it's more like a certain value in the realm.
              (str "No translation to " (format-id format) " for " (if (realm-inspection/realm? culprit)
                                                                     (realm-inspection/description culprit)
                                                                     (pr-str culprit))
                   (when-not (empty? realms-path)
                     (str ", at " (apply str (interpose ", " (map realm-inspection/description realms-path))))))
              "")
            {:type ::unsupported
             :format format
             :value culprit
             :path realms-path})))

(defn ^:no-doc exn-prepend-path [exn realm]
  ;; does not generate a new message yet (format is set later anyway)
  (ex-info (ex-message exn)
           (update (ex-data exn) :path
                   cons realm)))

(defn ^:no-doc exn-set-format [exn format]
  ;; generates a new message.
  (unsupported-exn format (:value (ex-data exn)) (:path (ex-data exn))))

(defn unsupported-exn? [e]
  (= ::unsupported (:type (ex-data e))))

(defmacro ^:no-doc wrap-unsupported-path [realm & body]
  `(try (do ~@body)
        (catch #?(:clj Exception :cljs :default) e#
          (if (unsupported-exn? e#)
            (throw (exn-prepend-path e# ~realm))
            (throw e#)))))

(defmacro ^:no-doc wrap-unsupported-format [format & body]
  `(try (do ~@body)
        (catch #?(:clj Exception :cljs :default) e#
          (if (unsupported-exn? e#)
            (throw (exn-set-format e# ~format))
            (throw e#)))))

(defn ^:no-doc get-default-translator! [format realm recurse]
  (assert (format? format))
  ;; Note: should throw (unsupported-exn) if realm not supported
  (let [default-fn (:get-default-translator format)]
    (default-fn realm recurse)))

(defn runtime-error [problem irritant]
  (ex-info problem {:type ::runtime
                    :irritant irritant}))

(defn runtime-error? [exn]
  (= ::runtime (:type (ex-data exn))))

;; TODO: utils to define formats.
