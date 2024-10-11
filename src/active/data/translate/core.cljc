(ns active.data.translate.core
  (:require [active.data.translate.lang :as lang]
            [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]))

(def ^:private meta-key ::translations)

(defn- set-translation-maker [realm lang translation]
  ;; TODO: compile realm?
  (realm/with-metadata realm meta-key
    (let [m (get (realm-inspection/metadata realm) meta-key)]
      (assoc m (lang/language-id lang) translation))))

(defn- get-translation-maker [realm lang]
  (get (get (realm-inspection/metadata realm) meta-key) (lang/language-id lang)))

(defn translate
  "Returns a copy of the given realm, with the given simple translator to the given target language attached."
  ([realm lang lens]
   (set-translation-maker realm lang (constantly lens)))
  ([realm lang lang-to-realm realm-to-lang]
   (translate realm lang (lang/translation lang-to-realm realm-to-lang))))

(defn translate-container
  "Returns a copy of the given realm, with a translator to the given
  target language attached.
  
  The translator is created by calling the given `make-translator`
  function with `recurse` function, which returns translators for the
  same language for other realms (including itself)."
  [realm lang make-translator]
  (set-translation-maker realm lang make-translator))

(defn- get-translator! [realm lang]
  ;; TODO we could also take a map/function from realms to translators instead of or in addition to the metadata.
  (let [realm (realm/compile realm) ;; in particular useful to translate records into realms.
        recurse-0 #(get-translator! % lang)
        ;; Note: when recurring with another realm, and that isn't
        ;; supported, then prepend this one in the path.
        recurse
        (fn [other-realm]
          (lang/wrap-unsupported-path realm
                                      (recurse-0 other-realm)))]
    (lang/wrap-unsupported-lang
     lang
     (or (when-let [maker (get-translation-maker realm lang)]
           (maker recurse))
         ;; Note: get-default should throw if realm is not supported by default
         (lang/get-default-translation! lang realm recurse)
         ;; this is just in case a lang default didn't catch all cases
         (throw (lang/unsupported-exn realm))))))

(defn translator-from [from-realm to-lang]
  (let [t (get-translator! from-realm to-lang)]
    (lang/get-realm-to-lang t)))

(defn translator-to [to-realm from-lang]
  (let [t (get-translator! to-realm from-lang)]
    (lang/get-lang-to-realm t)))

(defn translate-from [from-realm to-lang value]
  ;; pre: (realm/contains? value)
  ((translator-from from-realm to-lang) value))

(defn translate-to [to-realm from-lang value]
  ;; post: (realm/contains? result)
  ((translator-to to-realm from-lang) value))
