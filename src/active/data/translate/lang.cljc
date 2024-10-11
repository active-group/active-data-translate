(ns active.data.translate.lang
  (:require [active.clojure.lens :as lens]
            [active.data.realm.inspection :as realm-inspection]))

;; Rename language => format
;;        translation => translator

;; Translation

;; Translations are lenses, resp.
;; functions of the form
;; (fn ([lang-value] realm-value)
;;     ([_ realm-value] lang-value))

(defn translation
  ([lang-to-realm realm-to-lang]
   (lens/xmap lang-to-realm realm-to-lang)))

(defn realm-to-lang [translation v]
  (lens/shove nil translation v))

(defn lang-to-realm [translation v]
  (lens/yank v translation))

(defn get-realm-to-lang [translation]
  (partial realm-to-lang translation))

(defn get-lang-to-realm [translation]
  (partial lang-to-realm translation))

;; Lang

(defrecord ^:private Lang [id default-translations])

(def lang? (partial instance? Lang))

(defn ^:no-doc language
  ([id]
   (language id (constantly nil)))
  ([id default-translations]
   (Lang. id default-translations)))

#_(defmacro define-language
    ([name]
     `(def ~name
        (langage '~name)))
    ([name params & body]
     `(def ~name
        (langage '~name (fn ~params ~@body)))))

(defn language-id [lang]
  (assert (lang? lang))
  (:id lang))

(defn unsupported-exn
  ;; culprit should be a realm, or a simple value.
  ([culprit]
   (unsupported-exn culprit nil))
  ([culprit realms-path]
   (unsupported-exn nil culprit realms-path))
  ([lang culprit realms-path]
   (ex-info (if lang
              ;; TODO: if culprit is not a realm, the message should be different; then it's more like a certain value in the realm.
              (str "No translation to " (language-id lang) " for " (if (realm-inspection/realm? culprit)
                                                                     (realm-inspection/description culprit)
                                                                     (pr-str culprit))
                   (when-not (empty? realms-path)
                     (str ", at " (apply str (interpose ", " (map realm-inspection/description realms-path))))))
              "")
            {:type ::unsupported
             :lang lang
             :culprit culprit
             :path realms-path})))

(defn ^:no-doc exn-prepend-path [exn realm]
  (ex-info (ex-message exn)
           (update (ex-data exn) :path
                   cons realm)))

(defn ^:no-doc exn-set-lang [exn lang]
  (unsupported-exn lang (:culprit (ex-data exn)) (:path (ex-data exn))))

(defn unsupported-exn? [e]
  (= ::unsupported (:type (ex-data e))))

(defmacro ^:no-doc wrap-unsupported-path [realm & body]
  `(try (do ~@body)
        (catch #?(:clj Exception :cljs :default) e#
          (if (unsupported-exn? e#)
            (throw (exn-prepend-path e# ~realm))
            (throw e#)))))

(defmacro ^:no-doc wrap-unsupported-lang [lang & body]
  `(try (do ~@body)
        (catch #?(:clj Exception :cljs :default) e#
          (if (unsupported-exn? e#)
            (throw (exn-set-lang e# ~lang))
            (throw e#)))))

(defn get-default-translation! [lang realm recurse]
  (assert (lang? lang))
  ;; Note: should throw (unsupported-exn) if realm not supported
  (let [default-fn (:default-translations lang)]
    (default-fn realm recurse)))

(defn runtime-error [problem irritant]
  (ex-info problem {:type ::runtime
                    :irritant irritant}))
