(ns active.data.translate.core
  (:require [active.data.translate.format :as format #?@(:cljs [:include-macros true])]
            [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]
            [active.clojure.lens :as lens]))

(def ^:private meta-key ::translations)

(defn- set-translation-maker [realm format translation]
  ;; TODO: compile realm?
  (realm/with-metadata realm meta-key
    (let [m (get (realm-inspection/metadata realm) meta-key)]
      (assoc m (format/format-id format) translation))))

(defn- get-translation-maker [realm format]
  (get (get (realm-inspection/metadata realm) meta-key) (format/format-id format)))

(defn translate ;; TODO rename
  "Returns a copy of the given realm, with the given simple translator to the given target format attached."
  ([realm format lens]
   (set-translation-maker realm format (constantly lens)))
  ([realm format format-to-realm realm-to-format]
   (translate realm format (lens/xmap format-to-realm realm-to-format))))

(defn translate-container ;; TODO: rename
  "Returns a copy of the given realm, with a translator to the given
  target format attached.
  
  The translator is created by calling the given `make-translator`
  function with `recurse` function, which returns translators for the
  same format for other realms (including itself)."
  [realm format make-translator]
  (set-translation-maker realm format make-translator))

(defn- get-translator! [realm format]
  ;; TODO we could also take a map/function from realms to translators instead of or in addition to the metadata.
  (let [realm (realm/compile realm) ;; in particular useful to translate records into realms.
        recurse-0 #(get-translator! % format)
        ;; Note: when recurring with another realm, and that isn't
        ;; supported, then prepend this one in the path.
        recurse
        (fn [other-realm]
          (format/wrap-unsupported-path realm
                                        (recurse-0 other-realm)))]
    (format/wrap-unsupported-format
     format
     (or (when-let [maker (get-translation-maker realm format)]
           (maker recurse))
         ;; Note: get-default should throw if realm is not supported by default
         (format/get-default-translator! format realm recurse)
         ;; this is just in case a format default didn't catch all cases
         (throw (format/unsupported-exn realm))))))

(defn translator [from-realm to-format]
  (get-translator! from-realm to-format))

(defn translator-from [from-realm to-format]
  (let [t (translator from-realm to-format)]
    #(lens/shove nil t %)))

(defn translator-to [to-realm from-format]
  (let [t (translator to-realm from-format)]
    #(lens/yank % t)))

(defn translate-from [from-realm to-format value]
  ;; pre: (realm/contains? value)
  ((translator-from from-realm to-format) value))

(defn translate-to [to-realm from-format value]
  ;; post: (realm/contains? result)
  ((translator-to to-realm from-format) value))
