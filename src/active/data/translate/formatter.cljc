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

(defn- recursive-1 [realm f & args]
  (fn [resolve]
    (apply f (resolve realm) args)))

(defn- recursive-n [realms f & args]
  (fn [resolve]
    (apply f (mapv resolve realms) args)))

(defn ^:private id-translator [realm]
  (translator/translator (fn from-extern [v]
                           (if (realm/contains? realm v)
                             v
                             (throw (translator/format-error (str "Not in realm " (realm-inspection/description realm)) v))))
                         (fn to-extern [v] v) ;; TODO: realm-attach/fn?
                         realm))

(defn- sequence-of [item-realm]
  (recursive-1 item-realm
               (fn [translator]
                 (translator/translator (fn from-extern [v]
                                          (when-not (sequential? v)
                                            (throw (translator/format-error "Not a sequence" v)))
                                          (into (empty v) (map-indexed (fn [idx v]
                                                                         (translator/with-error-path idx
                                                                           ((translator/from-extern translator) v)))
                                                                       v)))
                                        (fn to-extern [v]
                                          (into (empty v) (map (translator/to-extern translator) v)))
                                        (realm/sequence-of (translator/external-realm translator))))))

(defn- map-of [key-realm value-realm]
  (recursive-n [key-realm value-realm]
               (fn [[key-translator value-translator]]
                 (translator/translator (fn from-extern [m]
                                          (when-not (map? m)
                                            (throw (translator/format-error "Not a map" m)))
                                          (-> (reduce-kv
                                               (fn [r k v]
                                                 (translator/with-error-path k
                                                   (assoc! r
                                                           ((translator/from-extern key-translator) k)
                                                           ((translator/from-extern value-translator) v))))
                                               (transient {})
                                               m)
                                              (persistent!)))
                                        (fn to-extern [m]
                                          (-> (reduce-kv
                                               (fn [r k v]
                                                 (assoc! r ((translator/to-extern key-translator) k)
                                                         ((translator/to-extern value-translator) v)))
                                               (transient {})
                                               m)
                                              (persistent!)))
                                        (realm/map-of (translator/external-realm key-translator)
                                                      (translator/external-realm value-translator))))))

(defn- map-with-keys [keys-realm-map]
  (recursive-n (vals keys-realm-map)
               (fn [value-translators]
                 (translator/translator (fn from-extern [m]
                                          (when-not (map? m)
                                            (throw (translator/format-error "Not a map" m)))
                                          (-> (map (fn [k t]
                                                     [k (translator/with-error-path k
                                                          ((translator/from-extern t) (get m k)))])
                                                   (keys keys-realm-map)
                                                   value-translators)
                                              (into {})))
                                        (fn to-extern [m]
                                          (-> (map (fn [k t]
                                                     [k ((translator/to-extern t) (get m k))])
                                                   (keys keys-realm-map)
                                                   value-translators)
                                              (into {})))
                                        (realm/map-with-keys (zipmap (keys keys-realm-map)
                                                                     (map translator/external-realm value-translators)))))))

(defn- set-of [item-realm]
  (recursive-1 item-realm
               (fn [translator]
                 (translator/translator (fn from-extern [v]
                                          (when-not (set? v)
                                            (throw (translator/format-error "Not a set" v)))
                                          (into #{} (map-indexed (fn [idx v]
                                                                   (translator/with-error-path idx
                                                                     ((translator/from-extern translator) v)))
                                                                 v)))
                                        (fn to-extern [v]
                                          (into #{} (map (translator/to-extern translator) v)))
                                        (realm/set-of (translator/external-realm translator))))))

(defn- tuple [& realms]
  (recursive-n realms
               (fn [translators]
                 (translator/translator (fn from-extern [v]
                                          (when-not (vector? v)
                                            (throw (translator/format-error "Not a vector" v)))
                                          (into [] (map-indexed (fn [idx [t v]]
                                                                  (translator/with-error-path idx
                                                                    ((translator/from-extern t) v)))
                                                                (map clj-vector translators v))))
                                        (fn to-extern [v]
                                          (into [] (map (fn [t v]
                                                          ((translator/to-extern t) v))
                                                        translators
                                                        v)))
                                        (apply realm/tuple (map translator/external-realm translators))))))

(defn identity
  "Returns an identity formatter for the given realm. The formatter
  throws if the external value is not contained in the realm."
  [realm]
  (cond
    (or (realm-inspection/string? realm)
        (realm-inspection/rational? realm)
        (realm-inspection/number? realm)
        (realm-inspection/char? realm)
        (realm-inspection/keyword? realm)
        (realm-inspection/symbol? realm)
        (realm-inspection/boolean? realm)
        (realm-inspection/uuid? realm)

        (realm-inspection/integer-from-to? realm)
        (realm-inspection/real-range? realm)

        (realm-inspection/map-with-tag? realm)
        (realm-inspection/enum? realm)
        (realm-inspection/from-predicate? realm))
    (simple (id-translator realm))

    (realm-inspection/sequence-of? realm)
    (sequence-of (realm-inspection/sequence-of-realm-realm realm))

    (realm-inspection/map-of? realm)
    (map-of (realm-inspection/map-of-realm-key-realm realm)
            (realm-inspection/map-of-realm-value-realm realm))

    (realm-inspection/map-with-keys? realm)
    (map-with-keys (realm-inspection/map-with-keys-realm-map realm))

    (realm-inspection/set-of? realm)
    (set-of (realm-inspection/set-of-realm-realm realm))

    (realm-inspection/tuple? realm)
    (apply tuple (realm-inspection/tuple-realm-realms realm))

    ;; intersection? function?

    :else nil))

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
