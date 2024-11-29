(ns active.data.translate.format
  (:require [active.data.realm.inspection :as realm-inspection]
            [active.data.realm :as realm]
            [active.clojure.lens :as lens])
  (:refer-clojure :exclude [format]))

;; Translators are lenses, resp.
;; functions of the form
;; (fn ([format-value] realm-value)
;;     ([_ realm-value] format-value))

;; A Formatter is a function that takes a 'recurse' function (which
;; returns translators for other realms) and returns a translator.

;; Formatters is a function/map that takes a realm and returns
;; formatter (or nil).

;; A Format is an identifier, together with default formatters

;; Format

(defrecord ^:private Format [id default-formatters])

(def format? (partial instance? Format))

(declare unsupported-exn)

(defn- compile-formatters [formatters]
  ;; Note: when using a map {record record-formatter}, the records have to be compile into a realm.
  ;; Convenient to do this automatically here (although quite excessive)
  (if (map? formatters)
    (->> formatters
         (map (fn [[k v]]
                [(realm/compile k) v]))
         (into {}))
    ;; if it's a function, it has to consider the difference between record and record-realms (and other things) itself.
    formatters))

(defn format
  ([id]
   (format id (fn [_recurse] (fn [realm] (throw (unsupported-exn realm))))))
  ([id default-formatters]
   (Format. id (compile-formatters default-formatters))))

(defn format-id [format]
  (assert (format? format))
  (:id format))

(defn- realm-path-str [realms-path]
  (apply str (interpose " > " (map realm-inspection/description realms-path))))

(defn unsupported-exn
  ;; culprit should be a realm, or a simple value.
  ([culprit]
   (unsupported-exn culprit nil))
  ([culprit realms-path]
   (unsupported-exn nil culprit realms-path))
  ([format culprit realms-path]
   (assert (every? realm-inspection/realm? realms-path) (first (remove realm-inspection/realm? realms-path)))
   (ex-info (str "No translation"
                 (if format (str " to " (format-id format)) "")
                 " for "
                 (if (realm-inspection/realm? culprit)
                   (str "realm " (realm-inspection/description culprit))
                   (pr-str culprit))
                 (when-not (empty? realms-path)
                   (str ", at " (realm-path-str realms-path))))
            {:type ::unsupported
             :format format
             :value culprit
             :path realms-path})))

(defn format-error
  ([problem irritant]
   (format-error problem irritant nil))
  ([problem irritant realms-path]
   (ex-info (str "Error formatting " (pr-str irritant) ": " problem
                 (when-not (empty? realms-path)
                   (str ", at " (realm-path-str realms-path))))
            {:type ::format-error
             :problem problem
             :irritant irritant
             :path realms-path})))

(defn format-error? [exn]
  (= ::format-error (:type (ex-data exn))))

(defn ^:no-doc exn-prepend-path [exn realm]
  (assert (realm-inspection/realm? realm) realm)
  ;; does not generate a new message yet (format is set later anyway)
  (ex-info (ex-message exn)
           (update (ex-data exn) :path
                   (fn [p] (cons realm p)))))

(defn ^:no-doc exn-set-format [exn format]
  ;; generates a new message.
  (unsupported-exn format (:value (ex-data exn)) (:path (ex-data exn))))

(defn unsupported-exn? [e]
  (= ::unsupported (:type (ex-data e))))

(defn- wrap-format-error* [thunk wrap]
  #?(:clj
     (try (thunk)
          (catch Exception e
            (if (format-error? e)
              (throw (wrap e))
              (throw e))))
     :cljs
     (try (thunk)
          (catch :default e
            (if (format-error? e)
              (throw (wrap e))
              (throw e))))))

(defn- wrap-unsupported-exn* [thunk wrap]
  #?(:clj
     (try (thunk)
          (catch Exception e
            (if (unsupported-exn? e)
              (throw (wrap e))
              (throw e))))
     :cljs
     (try (thunk)
          (catch :default e
            (if (unsupported-exn? e)
              (throw (wrap e))
              (throw e))))))

(defn ^:no-doc wrap-unsupported-path* [realm thunk]
  (wrap-unsupported-exn* thunk
                         (fn [e]
                           (exn-prepend-path e realm))))

(defn ^:no-doc wrap-unsupported-format* [format thunk]
  (wrap-unsupported-exn* thunk
                         (fn [e]
                           (exn-set-format e format))))

(defn- wrap-format-error-path** [realm thunk]
  (wrap-format-error* thunk
                      (fn [e]
                        (exn-prepend-path e realm))))

(defn- wrap-format-error-message** [thunk]
  ;; 'recreated' the exception message after paths have been added along the way.
  (wrap-format-error* thunk
                      (fn [e]
                        (let [d (ex-data e)]
                          (format-error (:problem d) (:irritant d) (:path d))))))

(defn ^:no-doc wrap-format-error-path* [realm translator]
  ;; Note: for large objects this will be a lot of try-catches, but what ya gonna do? (maybe a debug flag?)
  (lens/lens
   (fn [v]
     (wrap-format-error-path** realm #(lens/yank v translator)))
   (fn [d v]
     (wrap-format-error-path** realm #(lens/shove d translator v)))))

(defn ^:no-doc wrap-format-error-message* [translator]
  (lens/lens
   (fn [v]
     (wrap-format-error-message** #(lens/yank v translator)))
   (fn [d v]
     (wrap-format-error-message** #(lens/shove d translator v)))))

(defn ^:no-doc get-default-formatter [format realm]
  (assert (format? format))
  ;; Note: may throw (unsupported-exn) if realm not supported
  (let [default-fn (:default-formatters format)]
    (when-let [fmt (default-fn realm)]
      fmt)))

;; utils to define formats.

(defn combine-formatters [& formatters]
  ;; earlier ones have precendence
  (reduce (fn [res formatters]
            (if (and (map? res) (map? formatters))
              (merge formatters res)
              (fn [realm]
                (or (res realm)
                    (formatters realm)))))
          {}
          (map compile-formatters formatters)))

(defn simple [translator]
  ;; for those that don't need to recur
  (constantly translator))
