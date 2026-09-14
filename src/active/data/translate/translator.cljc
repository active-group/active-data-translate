(ns active.data.translate.translator
  "Translators represent the core functionality of translating between
  internal and external representations of values.

  Note that in general, translators should validate the external
  values, throwing [[format-error]] for invalid values, but should assume
  that internal values are valid, using [[active.data.realm.attach]] so add optional validation."
  (:require [active.data.record :as r #?@(:cljs [:include-macros true])]
            [clojure.string :as string])
  (:refer-clojure :rename {concat clj-concat}))

(r/def-record Translator
  [from-extern
   to-extern
   external-realm])

(defn translator [intern-from-extern intern-to-extern external-value-realm]
  (Translator from-extern intern-from-extern
              to-extern intern-to-extern
              external-realm external-value-realm))

#_(defn from-lens [lens external-realm] ;; would not support :keyword
    (Translator from-extern lens
                to-extern (fn [v] (lens nil v))
                external-realm external-realm))

(defn concat
  "Concatenates the given translators into one. The translators are applied 'from
  extern' right to left, and 'to extern' left to right."
  [translator & translators]
  (translator (apply comp (map from-extern (cons translator translators)))
              (apply comp (map to-extern (reverse (cons translator translators))))
              (external-realm (last (cons translator translators)))))

(def ^{:dynamic true :private true} *error-location* nil)

(defn- location-str [location]
  (if (or (nil? location) (seq? location))
    (string/join " > " (map str location))
    (str location)))

(defn format-error
  "Returns an exception given a problem description and the problematic value.

  Implicitly used the current error location (see [[with-error-location]] and [[add-error-path]], unless given explicitly."

  ([problem irritant]
   (format-error problem irritant
                 *error-location*))
  ([problem irritant location]
   (ex-info (str problem ": " (pr-str irritant) (if location (str ", at " (location-str location)) ""))
            {:type ::format-error
             :problem problem
             :irritant irritant
             :location location})))

(defn format-error? [e]
  (= ::format-error (:type (ex-data e))))

(defn set-error-location* [location f & args]
  (binding [*error-location* location]
    (apply f args)))

(defmacro with-error-location [location & body]
  `(set-error-location* ~location (fn [] ~@body)))

(defn add-error-path* [position f & args]
  (let [location *error-location*
        new-location (if (or (nil? location) (seq? location))
                       (clj-concat location [position])
                       (clj-concat [location] [position]))]
    (apply set-error-location* new-location
           f args)))

(defmacro with-error-path [position & body]
  `(add-error-path* ~position (fn [] ~@body)))
