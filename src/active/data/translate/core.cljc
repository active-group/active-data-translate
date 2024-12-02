(ns active.data.translate.core
  (:require [active.data.translate.format :as format]
            [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]
            [active.clojure.lens :as lens]))

(def ^:private meta-key ::translations)

(defn- set-realm-formatter [realm format formatter]
  (realm/with-metadata realm meta-key
    (let [m (get (realm-inspection/metadata realm) meta-key)]
      (assoc m (format/format-id format) formatter))))

(defn- get-realm-formatter [realm format]
  (get (get (realm-inspection/metadata realm) meta-key) (format/format-id format)))

(defn add-realm-translator
  "Returns a copy of the given realm, with the given simple translator to
the given target format attached."
  ([realm format format-to-realm realm-to-format]
   (add-realm-translator realm format (lens/xmap format-to-realm realm-to-format)))
  ([realm format lens]
   (set-realm-formatter (realm/compile realm) format (format/simple lens))))

(defn add-realm-formatter
  "Returns a copy of the given realm, with a formatter to the given
  target format attached."
  [realm format formatter]
  (set-realm-formatter (realm/compile realm) format formatter))

;; TODO: all the exception-handling is probably not very performant
;; (or is it?); it should probably be optional to have better error
;; message or better performance.

(defn- get-translator-0 [realm format]
  (let [realm (realm/compile realm)
        recurse-0 #(get-translator-0 % format)
        ;; Note: when recurring with another realm, and that isn't
        ;; supported, then prepend this one in the path.
        recurse
        (fn [other-realm]
          (format/wrap-unsupported-path* realm
                                         #(recurse-0 other-realm)))

        translator
        (or (let [formatter (or (get-realm-formatter realm format)
                                (format/get-default-formatter format realm))]
              (formatter recurse))
            ;; else:
            (throw (format/unsupported-exn realm)))]
    (format/wrap-format-error-path* realm translator)))

(defn- get-translator! [realm format]
  (format/wrap-unsupported-format*
   format
   #(format/wrap-format-error-message* (get-translator-0 realm format))))

(defn translator-lens [from-realm to-format]
  (get-translator! from-realm to-format))

(defn translator-from
  "Returns a function that translates values of the given realm into a
  formatted value, according to the given `format`."
  [from-realm to-format]
  ;; realm value => formatted value
  (let [t (translator-lens from-realm to-format)]
    #(lens/shove nil t %)))

(defn translator-to
  "Returns a function that translates formatted values into values of the
  given realm, according to the given `format`."
  [to-realm from-format]
  ;; formatted value => realm value
  (let [t (translator-lens to-realm from-format)]
    #(lens/yank % t)))
