(ns active.data.translate.formatter
  "A collection of some standard formatters that can be used to build formats."
  (:require [active.data.translate.format :as format]
            [active.data.realm.inspection :as realm-inspection]
            [active.data.realm :as realm]
            [active.clojure.lens :as lens]))

(defn simple
  "A simple (non recursive) formatter directly based on a translator lens."
  [translator]
  (constantly translator))

(def ^{:doc "Identity formatter, when no translation is needed for a realm."} id (simple lens/id))

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
        getter->keys (cond
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
                         (into {} (map vector getters spec)))

                       :else (assert false (str "Invalid record format spec: " spec)))
        expected-key? (set (vals getter->keys))
        ctor (realm-inspection/record-realm-constructor record-realm)
        fields (realm-inspection/record-realm-fields record-realm)]
    (fn [resolve]
      ;; adds the format translation based on the realm of the field
      (let [getter->realm-lens (->> fields
                                    (map (fn [field]
                                           (let [getter (realm-inspection/record-realm-field-getter field)]
                                             [getter
                                              ;; maybe use lens/id if it's realm/any? (i.e. undefined?)
                                              (resolve (realm-inspection/record-realm-field-realm field))])))
                                    (into {}))]
        (lens/xmap (fn to-realm [value]
                     (when-not (map? value)
                       (throw (format/format-error "Not a map" value)))

                     (when strict?
                       (when-not (empty? (remove expected-key? (keys value)))
                         (throw (format/format-error "Invalid key" (first (remove expected-key? (keys value)))))))

                     (apply ctor (map (fn [getter]
                                        (let [key (getter->keys getter)]
                                          (cond
                                            (contains? value key)
                                            (lens/yank (get value key) (getter->realm-lens getter))

                                            (contains? defaults getter)
                                            (get defaults getter)

                                            :else
                                            (throw (format/format-error "Missing key" (first (remove (set (keys value)) expected-key?)))))))
                                      getters)))
                   (fn from-realm [value]
                     (-> (reduce (fn [res getter]
                                   (let [key (getter->keys getter)]
                                     (assoc! res key
                                             (lens/shove nil (getter->realm-lens getter) (getter value)))))
                                 (transient {})
                                 getters)
                         (persistent!))))))))

(defn tagged-union-tuple
  "Formatter that distinguishes between different realms depending on the
   value of the first part of a tuple.

  Options are:
  
  `:fallback`: a function taking a tag and value, whose return value
  is used when the translated value has an unknown tag. If not given,
  a format-error is thrown.
  
  For example:
  ```
  (tagged-union-tuple {\"foo\" foo-realm})
  ```
  returns a formatter that can be used for a union realm that contains `foo-realm`, where the formatted values look like:
  ```
  [\"foo\" <foo-value>]
  ```
  "
  [tag-realm-map & {fallback :fallback}]
  (let [tag-realm-map (->> tag-realm-map
                           (map (fn [[k r]]
                                  [k (realm/compile r)]))
                           (into {}))]
    (fn [resolve]
      (let [tag-translator-map (->> tag-realm-map
                                    (map (fn [[k realm]]
                                           [k (resolve realm)]))
                                    (into {}))]
        (lens/xmap (fn to-realm [value]
                     (when-not (and (vector? value) (= 2 (count value)))
                       (throw (format/format-error "Not a tuple of length 2" value)))
                     (let [[tag content] value]
                       (if-not (contains? tag-translator-map tag)
                         (if (some? fallback)
                           (fallback tag content)
                           (throw (format/format-error "Unexpected tag" (first value))))
                         (lens/yank (second value) (get tag-translator-map (first value))))))
                   (fn from-realm [content]
                     (if-let [[_ result] (reduce-kv (fn [_res tag realm]
                                                      (when (realm/contains? realm content)
                                                        (reduced [:ok [tag (lens/shove nil (get tag-translator-map tag) content)]])))
                                                    nil
                                                    tag-realm-map)]
                       result
                       (throw (format/format-error "Value not contained in any of the realms" content)))))))))

(defn tagged-union-map
  "Formatter that distinguishes between different realms depending on a
   tag value in a map.

  Options are:

  `:fallback`: a function taking a tag and value, whose return value
  is used when the translated value has an unknown tag. If not given,
  a format-error is thrown.
  
  For example:
  ```
  (tagged-union-map :tag :value {\"foo\" foo-realm})
  ```
  returns a formatter that can be used for a union realm that contains `foo-realm`, where the formatted values look like:
  ```
  {:tag \"foo\" :value <foo-value>}
  ```
  "
  [tag-key content-key tag-realm-map & {fallback :fallback}]
  (let [tup (tagged-union-tuple tag-realm-map
                                :fallback fallback)]
    (fn [resolve]
      (let [lens (tup resolve)]
        (lens/xmap (fn to-realm [value]
                     (when-not (map? value)
                       (throw (format/format-error "Not a map" value)))
                     (when-not (contains? value tag-key)
                       (throw (format/format-error "Missing tag" value)))
                     (when-not (contains? value content-key)
                       (throw (format/format-error "Missing content" value)))

                     (lens/yank [(get value tag-key) (get value content-key)]
                                lens))
                   (fn from-realm [value]
                     (let [[tag content] (lens/shove nil lens value)]
                       {tag-key tag
                        content-key content})))))))

(defn constants
  "Formatter that translates fixed values, like in a realm/enum

  Options are:

  `:fallback`: a function called on unknown values. If not given, a
  format-error is thrown in those case.

  Usage:
  ```
  (constants {:foo \"foo\"})
  ```
  "
  [intern-extern-map & {fallback :fallback}]
  (let [extern-intern-map (->> intern-extern-map
                               (map (fn [[k v]]
                                      [v k]))
                               (into {}))]
    (assert (= (count extern-intern-map)
               (count intern-extern-map))
            "Duplicate value in contants map")
    (simple
     (lens/xmap (fn to-realm [value]
                  (let [r (get extern-intern-map value ::not-found)]
                    (if (= ::not-found r)
                      (if fallback
                        (fallback value)
                        (throw (format/format-error "Undefined constant" value)))
                      r)))
                (fn from-realm [value]
                  (let [r (get intern-extern-map value ::not-found)]
                    ;; Note: fallback is not used here intentionally.
                    (when (= ::not-found r)
                      (throw (format/format-error "Unexpected constant" value)))
                    r))))))

