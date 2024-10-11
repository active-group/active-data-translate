(ns active.data.translate.format
  (:require [active.data.realm.inspection :as realm-inspection]
            [active.data.realm :as realm]
            [active.clojure.lens :as lens])
  (:refer-clojure :exclude [format]))

;; Translators are lenses, resp.
;; functions of the form
;; (fn ([format-value] realm-value)
;;     ([_ realm-value] format-value))

;; A Formatter is a function that takes a 'recurse' function (which
;; returns translators for other realms) and returns a translator.

;; Formatters is a function/map that takes a realm and returns
;; formatter (or nil).
;; TODO: rename formatters... to format?

;; A Format is an identifier, together with default formatters

;; Format

(defrecord ^:private Format [id default-formatters])

(def format? (partial instance? Format))

(declare unsupported-exn)

(defn format
  ([id]
   (format id (fn [_recurse] (fn [realm] (throw (unsupported-exn realm))))))
  ([id default-formatters]
   (Format. id default-formatters)))

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
   (assert (every? realm-inspection/realm? realms-path) (first (remove realm-inspection/realm? realms-path)))
   (ex-info (if format
              ;; TODO: if culprit is not a realm, the message should be different; then it's more like a certain value in the realm.
              (str "No translation to " (format-id format) " for realm " (if (realm-inspection/realm? culprit)
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
  (assert (realm-inspection/realm? realm) realm)
  ;; does not generate a new message yet (format is set later anyway)
  (ex-info (ex-message exn)
           (update (ex-data exn) :path
                   (fn [p] (cons realm p)))))

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
  (let [default-fn (:default-formatters format)]
    (when-let [fmt (default-fn realm)]
      (fmt recurse))))

(defn runtime-error [problem irritant]
  (ex-info problem {:type ::runtime
                    :irritant irritant}))

(defn runtime-error? [exn]
  (= ::runtime (:type (ex-data exn))))

;; utils to define formats.

(defn combine-formatters [& formatters]
  ;; earlier ones have precendence
  (reduce (fn [res formatters]
            (if (and (map? res) (map formatters))
              (merge formatters res)
              (fn [realm]
                (or (res realm)
                    (formatters realm)))))
          {}
          formatters))

(defn simple-formatters [realm-translator-map]
  ;; for those that don't need to recur
  (into {} (map (fn [[realm translator]]
                  [realm (constantly translator)])
                realm-translator-map)))

(defn ^:no-doc record-map-formatters-1
  "(record-map-formatters MyRecord
                         {:foo my-rec-foo
                          :bar my-rec-bar})"
  [record-realm key-getter-map]
  (let [record-realm (realm/compile record-realm)
        getters (->> (realm-inspection/record-realm-fields record-realm)
                     (map realm-inspection/record-realm-field-getter))
        getter? (set getters)]
    ;; must be a map from keywords to record getters
    (assert (every? keyword? (keys key-getter-map)))
    (assert (every? getter? (vals key-getter-map)))
    ;; and not the same getter multiple times.
    (assert (apply distinct? getters))
    ;; Not all getters must be used - those fields will be set to nil
    (let [getter->key (into {} (map (fn [[key getter]]
                                      [getter key])
                                    key-getter-map))
          ctor (realm-inspection/record-realm-constructor record-realm)]
      {record-realm (fn [recurse]
                      (let [getter->lens (->> (realm-inspection/record-realm-fields record-realm)
                                              (map (fn [field]
                                                     [(realm-inspection/record-realm-field-getter field)
                                                      (recurse (realm-inspection/record-realm-field-realm field))]))
                                              (into {}))]
                        (lens/xmap (fn to-realm [value]
                                     (apply ctor (map (fn [getter]
                                                        (if (contains? getter->key getter)
                                                          (lens/yank (get value (getter->key getter))
                                                                     (getter->lens getter))
                                                          ;; if getter not used, insert nil
                                                          nil))
                                                      getters)))
                                   (fn from-realm [value]
                                     (into {} (map (fn [[key getter]]
                                                     [key (lens/shove nil (getter->lens getter) (getter value))])
                                                   key-getter-map))))))})))

(defn record-map-formatters [records-map]
  (apply combine-formatters (map (fn [[record map-spec]]
                                   (record-map-formatters-1 record map-spec))
                                 records-map)))
