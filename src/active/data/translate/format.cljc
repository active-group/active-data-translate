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

(defn- compile-formatters [formatters]
  ;; Note: when using a map {record record-formatter}, the records have to be compile into a realm.
  ;; Convenient to do this automatically here (although quite excessive)
  (if (map? formatters)
    (->> formatters
         (map (fn [[k v]]
                [(realm/compile k) v]))
         (into {}))
    ;; if it's a function, it has to consider the difference between record and record-realms (and other things) itself.
    formatters))

(defn format
  ([id]
   (format id (fn [_recurse] (fn [realm] (throw (unsupported-exn realm))))))
  ([id default-formatters]
   (Format. id (compile-formatters default-formatters))))

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

(defn ^:no-doc wrap-unsupported-exn* [thunk wrap]
  #?(:clj
     (try (thunk)
          (catch Exception e
            (if (unsupported-exn? e)
              (throw (wrap e))
              (throw e))))
     :cljs
     (try (thunk)
          (catch :default e
            (if (unsupported-exn? e)
              (throw (wrap e))
              (throw e))))))

(defmacro ^:no-doc wrap-unsupported-path [realm & body]
  `(wrap-unsupported-exn* (fn [] ~@body)
                          (fn [e#]
                            (exn-prepend-path e# ~realm))))

(defmacro ^:no-doc wrap-unsupported-format [format & body]
  `(wrap-unsupported-exn* (fn [] ~@body)
                          (fn [e#]
                            (exn-set-format e# ~format))))

(defn ^:no-doc get-default-translator! [format realm recurse]
  (assert (format? format))
  ;; Note: should throw (unsupported-exn) if realm not supported
  (let [default-fn (:default-formatters format)]
    (when-let [fmt (default-fn realm)]
      (fmt recurse))))

(defn runtime-error [problem irritant] ;; TODO: rename; value-error, validation-error?
  (ex-info problem {:type ::runtime
                    :irritant irritant}))

(defn runtime-error? [exn]
  (= ::runtime (:type (ex-data exn))))

;; utils to define formats.

(defn combine-formatters [& formatters]
  ;; earlier ones have precendence
  (reduce (fn [res formatters]
            (if (and (map? res) (map? formatters))
              (merge formatters res)
              (fn [realm]
                (or (res realm)
                    (formatters realm)))))
          {}
          (map compile-formatters formatters)))

(defn simple [translator]
  ;; for those that don't need to recur
  (constantly translator))

(defn record-map
  "(record-map MyRecord
               {my-rec-foo :foo 
                my-rec-bar :bar})

  or

  (record-map MyRecord [:foo :bar])"

  [record spec]
  (let [record-realm (realm/compile record)
        getters (->> (realm-inspection/record-realm-fields record-realm)
                     (map realm-inspection/record-realm-field-getter))
        getter->lens (cond
                       ;; {field-getter -> lens}
                       (map? spec) spec
                       ;; Alternative: [:foo :bar] that relies on the order  (lacks the reference to the field; harder to read and refactor)
                       (vector? spec)
                       (into {} (map vector getters spec))

                       :else (assert false (str "Invalid record format spec: " spec)))
        getter? (set getters)]
    (assert (every? getter? (keys getter->lens))
            (str "No such field " (first (remove getter? (keys getter->lens))) " in " (realm-inspection/description record-realm)))
    (assert (= (count getter->lens) (count getters))
            (str "Missing field: " (first (remove #(contains? getter->lens %) getters)) " for " (realm-inspection/description record-realm)))
    ;; Not all getters must be used - those fields will be set to nil
    (let [ctor (realm-inspection/record-realm-constructor record-realm)
          fields (realm-inspection/record-realm-fields record-realm)]
      (fn [recurse]
        ;; adds the format translation based on the realm of the field
        (let [getter->realm-lens (->> fields
                                      (map (fn [field]
                                             (let [getter (realm-inspection/record-realm-field-getter field)]
                                               [getter
                                                ;; OPT: if getter->lens are all keywords, then from-realm can be optimized
                                                (lens/>> (getter->lens getter)
                                                         ;; maybe use lens/id if it's realm/any? (i.e. undefined?)
                                                         (recurse (realm-inspection/record-realm-field-realm field)))])))
                                      (into {}))]
          (lens/xmap (fn to-realm [value]
                       ;; TODO: runtime-error is value contains more keys. (but then 'lenses' must be keys)
                       (apply ctor (map (fn [getter]
                                          (lens/yank value (getter->realm-lens getter)))
                                        getters)))
                     (fn from-realm [value]
                       (reduce (fn [res getter]
                                 (lens/shove res (getter->realm-lens getter) (getter value)))
                               {}
                               getters))))))))
