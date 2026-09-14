(ns active.data.translate.formatter
  "A formatter is a function [resolve] => translator, which create a translator,
  potentially based on other translators for a realm returned by the resolve function.

  This namespace contains a collection of some standard formatters and utilities to build others."
  (:require [active.data.translate.translator :as translator #?@(:cljs [:include-macros true])]
            [active.data.realm.inspection :as realm-inspection]
            [active.data.realm :as realm])
  (:refer-clojure :exclude [identity string char keyword]
                  :rename {vector clj-vector}))

(defn simple
  "A simple (non recursive) formatter."
  [translator]
  (fn [_resolve] translator))

#_(defn- recursive-1 [realm f & args]
    (fn [resolve]
      (apply f (resolve realm) args)))

#_(defn recursive-n [realms f & args]
    (fn [resolve]
      (apply f (mapv resolve realms) args)))

(defn ^:private id-translator [realm]
  (translator/translator (fn from-extern [v]
                           (if (realm/contains? realm v)
                             v
                             (throw (translator/format-error (str "Not in realm " (realm-inspection/description realm)) v))))
                         (fn to-extern [v] v) ;; TODO: realm-attach/fn?
                         realm))

(defn identity
  "Returns an identity formatter for the given realm, when no actual
  translation is needed. The formatter throws if the external value is not
  contained in the realm."
  [realm]
  (simple (id-translator realm)))

;; (def ^{:doc "Formatter that keeps strings as they are."} string (identity realm/string))

;; (def ^{:doc "Formatter that keeps chars as they are."} char (identity realm/char))

;; (def ^{:doc "Formatter that keeps keywords as they are."} keyword (identity realm/keyword))

;; (def ^{:doc "Formatter that keeps symbols as they are."} symbol (identity realm/symbol))

;; (def ^{:doc "Formatter that keeps booleans as they are."} boolean (identity realm/boolean))

;; TODO? integer(from to)
;; TOOD? real(from to)

;; TODO? utils for intersection, sequence-of, set-of, map-of, map-with-tag, map-with-keys, tuples,

#_(defn vector [realm]
    (recursive-1 realm (fn [translator]
                         (translator/translator (fn from-extern [v]
                                                  (when-not (vector? v)
                                                    (throw (translator/format-error "Not a vector" v)))
                                                  (mapv (translator/from-extern translator) v))
                                                (fn to-extern [v]
                                                  (mapv (translator/to-extern translator) v))
                                                (realm/sequence-of (translator/external-realm translator))))))

(defn record-map
  "Formatter to represent a record as a map with explicit keys.

  Options are:

  `:strict?`: if set then a format-error is thrown when the map contains
  unknown keys. By default they are ignored.

  `:defaults`: a map of defaults for record fields. If none is given then a
  format-error is thrown if the parsed map does not contain a defined key.

  Usage:
  ```
  (record-map MyRecord
              {my-rec-foo :foo 
               my-rec-bar :bar}
              :defaults {my-rec-foo \"value\"})
  ```
  
  or

  ```
  (record-map MyRecord [:foo :bar])
  ```
  "
  [record spec & {strict? :strict? defaults :defaults}]
  (let [record-realm (realm/compile record)
        record-name (realm-inspection/record-realm-name record-realm)
        ;; Note: 'getters' is in the order expected by the constructor.
        getters (->> (realm-inspection/record-realm-fields record-realm)
                     (map realm-inspection/record-realm-field-getter))
        getter->key (cond
                       ;; {field-getter -> key}
                      (map? spec)
                      (let [getter? (set getters)]
                        (assert (every? getter? (keys spec))
                                (str "No such field " (first (remove getter? (keys spec))) " in " record-name))
                        (assert (= (count spec) (count getters))
                                (str "Missing field: " (first (remove #(contains? spec %) getters)) " for " record-name))
                        spec)
                       ;; Alternative: [:foo :bar] that relies on the order  (lacks the reference to the field; harder to read and refactor)
                      (vector? spec)
                      (do
                        (assert (= (count spec) (count getters))
                                (str "Record " record-name " has " (count getters) " fields. Given: " (count spec)))
                        (into {} (map clj-vector getters spec)))

                      :else (assert false (str "Invalid record format spec: " spec)))
        expected-key? (set (vals getter->key))
        ctor (realm-inspection/record-realm-constructor record-realm)
        fields (realm-inspection/record-realm-fields record-realm)]
    (fn [resolve]
      ;; adds the format translation based on the realm of the field
      (let [getter->translator (->> fields
                                    (map (fn [field]
                                           (let [getter (realm-inspection/record-realm-field-getter field)
                                                 field-realm (realm-inspection/record-realm-field-realm field)]
                                             [getter
                                              ;; if field-realm is realm/any, we assume the record was defined without a realm;
                                              ;; we then use an unchecked identity here, so that the format does not need to define a translation for realm/any.
                                              (if false #_(realm-inspection/any? field-realm) ;; TODO: with new active-data version
                                                  (identity realm/any)
                                                  (resolve field-realm))])))
                                    (into {}))
            getter->from-extern (->> getter->translator
                                     (map (fn [[getter translator]]
                                            [getter (translator/from-extern translator)]))
                                     (into {}))
            getter->to-extern (->> getter->translator
                                   (map (fn [[getter translator]]
                                          [getter (translator/to-extern translator)]))
                                   (into {}))]
        (translator/translator (fn from-extern [value]
                                 (when-not (map? value)
                                   (throw (translator/format-error "Not a map" value)))

                                 (when strict?
                                   (when-not (empty? (remove expected-key? (keys value)))
                                     (throw (translator/format-error "Invalid key" (first (remove expected-key? (keys value)))))))

                                 (apply ctor (map (fn [getter]
                                                    (let [key (getter->key getter)]
                                                      (cond
                                                        (contains? value key)
                                                        (translator/with-error-path key
                                                          ((getter->from-extern getter)
                                                           (get value key)))

                                                        (contains? defaults getter)
                                                        (get defaults getter)

                                                        :else
                                                        (throw (translator/format-error "Missing key" (first (remove (set (keys value)) expected-key?)))))))
                                                  getters)))
                               (fn to-extern [value] ;; TODO: realm-attach/fn?
                                 (-> (reduce (fn [res getter]
                                               (let [key (getter->key getter)]
                                                 (assoc! res key
                                                         ((getter->to-extern getter) (getter value)))))
                                             (transient {})
                                             getters)
                                     (persistent!)))
                               (realm/map-with-keys
                                ;; TODO: can we consider strict?
                                (into {}
                                      (map (fn [getter]
                                             [(getter->key getter)
                                              (cond-> (translator/external-realm (getter->translator getter))
                                                (contains? defaults getter) (realm/optional))])
                                           getters))))))))

(defn tagged-union-tuple
  "Formatter that distinguishes between different realms depending on the
   value of the first part of a tuple.

  For example:
  ```
  (tagged-union-tuple {\"foo\" foo-realm})
  ```
  returns a formatter that can be used for a union realm that contains `foo-realm`, where the formatted values look like:
  ```
  [\"foo\" <foo-value>]
  ```
  "
  [tag-realm-map]
  (let [tag-realm-map (->> tag-realm-map
                           (map (fn [[k r]]
                                  [k (realm/compile r)]))
                           (into {}))]
    (fn [resolve]
      (let [tag-translator-map (->> tag-realm-map
                                    (map (fn [[k realm]]
                                           [k (resolve realm)]))
                                    (into {}))]
        (translator/translator (fn to-realm [value]
                                 (when-not (and (vector? value) (= 2 (count value)))
                                   (throw (translator/format-error "Not a tuple of length 2" value)))
                                 (let [[tag content] value]
                                   (if-not (contains? tag-translator-map tag)
                                     (throw (translator/format-error "Unexpected tag" (first value)))
                                     ((translator/from-extern (get tag-translator-map (first value)))
                                      content))))
                               (fn from-realm [content] ;; TODO: attach/fn ?
                                 (if-let [[_ result] (reduce-kv (fn [_res tag realm]
                                                                  (when (realm/contains? realm content)
                                                                    (reduced [:ok [tag ((translator/to-extern (get tag-translator-map tag))
                                                                                        content)]])))
                                                                nil
                                                                tag-realm-map)]
                                   result
                                   (assert false (str "Value not contained in any of the realms: " content))))
                               (realm/tuple (apply realm/enum (keys tag-realm-map))
                                            (apply realm/union (vals tag-realm-map))))))))

(defn tagged-union-map
  "Formatter that distinguishes between different realms depending on a
   tag value in a map.

  For example:
  ```
  (tagged-union-map :tag :value {\"foo\" foo-realm})
  ```
  returns a formatter that can be used for a union realm that contains `foo-realm`, where the formatted values look like:
  ```
  {:tag \"foo\" :value <foo-value>}
  ```
  "
  [tag-key content-key tag-realm-map]
  (let [tup (tagged-union-tuple tag-realm-map)]
    (fn [resolve]
      (let [trans (tup resolve)]
        (translator/translator (fn to-realm [value]
                                 (when-not (map? value)
                                   (throw (translator/format-error "Not a map" value)))
                                 (when-not (contains? value tag-key)
                                   (throw (translator/format-error "Missing tag" value)))
                                 (when-not (contains? value content-key)
                                   (throw (translator/format-error "Missing content" value)))

                                 ((translator/from-extern trans) [(get value tag-key) (get value content-key)]))
                               (fn from-realm [value] ;; TODO: attach/fn ?
                                 (let [[tag content] ((translator/to-extern trans) value)]
                                   {tag-key tag
                                    content-key content}))
                               (realm/map-with-keys {tag-key (apply realm/enum (keys tag-realm-map))
                                                     content-key (apply realm/union (vals tag-realm-map))}))))))

(defn constants
  "Formatter that translates fixed values, like in a realm/enum

  Usage:
  ```
  (constants {:foo \"foo\"})
  ```
  "
  ([intern-extern-map]
   (constants intern-extern-map (apply realm/enum (vals intern-extern-map))))
  ([intern-extern-map external-realm]
   (let [extern-intern-map (->> intern-extern-map
                                (map (fn [[k v]]
                                       [v k]))
                                (into {}))]
     (assert (= (count extern-intern-map)
                (count intern-extern-map))
             "Duplicate value in contants map")
     (simple
      (translator/translator (fn from-extern [value]
                               (let [r (get extern-intern-map value ::not-found)]
                                 (if (= ::not-found r)
                                   (throw (translator/format-error "Invalid value" value))
                                   r)))
                             (fn to-extern [value]  ;; TODO: realm/fn?
                               (let [r (get intern-extern-map value ::not-found)]
                                 (if (= ::not-found r)
                                   (assert false value)
                                   r)))
                             external-realm)))))
