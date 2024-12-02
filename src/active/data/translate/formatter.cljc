(ns active.data.translate.formatter
  "A collection of some standard formatters that can be used to build formats."
  (:require [active.data.translate.format :as format]
            [active.data.realm.inspection :as realm-inspection]
            [active.data.realm :as realm]
            [active.clojure.lens :as lens]))

(defn record-map
  "Formatter to represent a record as a map with explicit keys.

  Usage:
  ```
  (record-map MyRecord
              {my-rec-foo :foo 
               my-rec-bar :bar})
  ```
  
  or

  ```
  (record-map MyRecord [:foo :bar])
  ```
  
  "

  [record spec]
  (let [record-realm (realm/compile record)
        getters (->> (realm-inspection/record-realm-fields record-realm)
                     (map realm-inspection/record-realm-field-getter))
        getter->keys (cond
                       ;; {field-getter -> key}
                       (map? spec) spec
                       ;; Alternative: [:foo :bar] that relies on the order  (lacks the reference to the field; harder to read and refactor)
                       (vector? spec)
                       (into {} (map vector getters spec))

                       :else (assert false (str "Invalid record format spec: " spec)))
        expected-key? (set (vals getter->keys))
        getter? (set getters)]
    (assert (every? getter? (keys getter->keys))
            (str "No such field " (first (remove getter? (keys getter->keys))) " in " (realm-inspection/description record-realm)))
    (assert (= (count getter->keys) (count getters))
            (str "Missing field: " (first (remove #(contains? getter->keys %) getters)) " for " (realm-inspection/description record-realm)))
    ;; Not all getters must be used - those fields will be set to nil
    (let [ctor (realm-inspection/record-realm-constructor record-realm)
          fields (realm-inspection/record-realm-fields record-realm)]
      (fn [resolve]
        ;; adds the format translation based on the realm of the field
        (let [getter->realm-lens (->> fields
                                      (map (fn [field]
                                             (let [getter (realm-inspection/record-realm-field-getter field)]
                                               [getter
                                                (lens/>> (lens/member (getter->keys getter))
                                                         ;; maybe use lens/id if it's realm/any? (i.e. undefined?)
                                                         (resolve (realm-inspection/record-realm-field-realm field)))])))
                                      (into {}))]
          (lens/xmap (fn to-realm [value]
                       (when-not (map? value)
                         (throw (format/format-error "Not a map" value)))
                       (when-not (empty? (remove expected-key? (keys value)))
                         ;; allow less keys for now (the field realm may still complain about nil)
                         ;; TODO: really? make it optional?
                         (throw (format/format-error "Invalid key" (first (remove expected-key? (keys value))))))
                       (apply ctor (map (fn [getter]
                                          (lens/yank value (getter->realm-lens getter)))
                                        getters)))
                     (fn from-realm [value]
                       (when-not (realm/contains? record-realm value)
                         (throw (format/format-error "Not this record" value)))
                       (reduce (fn [res getter]
                                 (lens/shove res (getter->realm-lens getter) (getter value)))
                               {}
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
        (lens/xmap (fn to-realm [value]
                     (when-not (or (vector? value) (not= 2 (count value)))
                       (throw (format/format-error "Not a tuple of length 2" value)))
                     (when-not (contains? tag-translator-map (first value))
                       (throw (format/format-error "Unexpected tag" (first value))))
                     (lens/yank (second value) (get tag-translator-map (first value))))
                   (fn from-realm [content]
                     (if-let [[_ result] (reduce-kv (fn [_res tag realm]
                                                      (when (realm/contains? realm content)
                                                        (reduced [:ok [tag (lens/shove nil (get tag-translator-map tag) content)]])))
                                                    nil
                                                    tag-realm-map)]
                       result
                       (throw (format/format-error "Value not contained in any of the realms" content)))))))))

(defn tagged-union-map
  "Formatter that distinguishes between different realms depending on a tag value in a map.

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

  Usage:
  ```
  (constants {:foo \"foo\"})
  ```
  "
  [intern-extern-map]
  (let [extern-intern-map (->> intern-extern-map
                               (map (fn [[k v]]
                                      [v k]))
                               (into {}))]
    (assert (= (count extern-intern-map)
               (count intern-extern-map))
            "Duplicate value in contants map")
    (fn [_resolve]
      (lens/xmap (fn to-realm [value]
                   (let [r (get extern-intern-map value ::not-found)]
                     (when (= ::not-found r)
                       (throw (format/format-error "Undefined constant" value)))
                     r))
                 (fn from-realm [value]
                   (let [r (get intern-extern-map value ::not-found)]
                     (when (= ::not-found r)
                       (throw (format/format-error "Unexpected constant" value)))
                     r))))))

