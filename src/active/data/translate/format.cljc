(ns active.data.translate.format
  "A format is a function [realm] => formatter, which should return nil for unsupported realms.

  This namespace contains utilities to create and use such functions."

  (:require [active.data.realm.inspection :as realm-inspection]
            [active.data.realm :as realm]
            [active.data.translate.formatter :as formatter]
            [active.data.translate.translator :as translator])
  (:refer-clojure :exclude [empty identity]))

(defn- compile-formatters-map [formatters]
  ;; Note: when using a map {record record-formatter}, the records have to be compiled into realms.
  ;; Convenient to do this automatically here (although quite excessive)
  (if (map? formatters)
    (->> formatters
         (map (fn [[k v]]
                [(realm/compile k) v]))
         (into {}))
    ;; if it's a function, it has to consider the difference between records and record-realms (and other things) itself.
    formatters))

(def ^{:doc "The empty format that supports nothing."} empty {})

(def ^{:doc "A format that keeps all values as they are.

  Note that this still resolves any nested realms, so if you combine this with a format that for example translates integers, you get sequences of integers for free."} identity
  (fn [realm]
    (formatter/identity realm)))

(defn combine-formats
  "Combines multiple formats into one. Earlier formatters for a realm take precedence over later ones."
  [& formats]
  (reduce (fn [res format]
            (if (and (map? res) (map? format))
              (merge format res)
              (fn [realm]
                (or (res realm)
                    (format realm)))))
          empty
          (map compile-formatters-map formats)))

(defn unsupported-exn [realm]
  (ex-info (str "Unsupported realm: " (realm-inspection/description realm))
           {:type ::unsupported
            :realm realm}))

(defn unsupported-exn? [e]
  (= ::unsupported (:type (ex-data e))))

(defn- get-realm [realm]
  (let [realm (realm/compile realm)]
    (cond
      (realm-inspection/named? realm) (realm-inspection/named-realm-realm realm)
      (realm-inspection/delayed? realm) (force (realm-inspection/delayed-realm-delay realm))
      :else realm)))

(defn get-translator
  "Resolves the translator for the given realm in the given format, or throws [[unsupported-exn]]."
  [format realm]
  (assert (or (fn? format) (ifn? format)) format)
  (let [realm (get-realm realm)
        format (compile-formatters-map format)
        _ (assert (realm-inspection/realm? realm) realm)
        resolve (fn resolve [realm]
                  (assert (realm-inspection/realm? realm) realm)
                  (if-let [formatter (format realm)]
                    (do (assert (or (fn? formatter) (ifn? formatter)) formatter)
                        (formatter resolve))
                    (throw (unsupported-exn realm))))]
    (resolve realm)))

(defn- concat-formats-2 [f1 f2]
  (fn [realm]
    (let [t1 (get-translator f1 realm)
          t2 (get-translator f2 (translator/external-realm t1))]
      (translator/concat-translators t1 t2))))

(defn concat-formats
  "Concatenate the given formats, ie. translate a value through all of
   the given formats, left to right for 'to external', and right to left
   for 'from external'."
  [& formats]
  (if (empty? formats)
    empty
    (if (empty? (rest formats))
      (first formats)
      (reduce concat-formats-2
              (first formats)
              (rest formats)))))

