(ns active.data.translate.reitit
  (:require [active.data.translate.core :as core]
            [active.data.realm.validation :as realm-validation]
            [active.data.realm.inspection :as realm-inspection]
            [active.data.translate.common :as common]
            [active.data.realm :as realm]
            [active.data.record :as record]
            [reitit.coercion :as coercion]
            [clojure.set :as set]))

(defrecord ^:private MapModel [map open?])

(defrecord ^:private RealmModel [to from])

(defn- realm-model [format realm]
  (let [realm (realm/compile realm)]
    (map->RealmModel
     {:from (core/translator-from realm format)
      :to (let [to (core/translator-to realm format)]
            (fn [value]
              ;; FIXME: checking is not thread-safe; but validator only checks one level; need a deep check here.
              ;; OR: don't do any checking?
              (try (realm-validation/checking (to value))
                   (catch Exception e
                     (coercion/map->CoercionError
                      {:realm (realm-inspection/description realm)
                       :problems [(:error (ex-data e))]})))))})))

(defn- compile-model [body-format string-format model _name]
  ;; Note: model may be {:foo realm} for query and path parameters
  ;; 'open-model' may be called too (for path or for what?)
  (cond
    (or (realm-inspection/realm? model) (record/record? model))
    (realm-model body-format model)

    (map? model)
    (do (assert (every? keyword? (keys model)))
        (MapModel. (->> model
                        (map (fn [[key model]]
                               [key (realm-model string-format model)]))
                        (into {}))
                   false))

    :else
    (assert false (str "Invalid model: " (pr-str model)))))

(defn- convert-to-model [model value format]
  (assert (nil? format) (str "TODO: What does " format "mean?"))

  (condp instance? model
    RealmModel
    ((:to model) value)

    MapModel
    (let [known (reduce-kv (fn [res key model]
                             (let [r (convert-to-model model (get value key) nil)]
                               (if (coercion/error? r)
                                 (reduced r)
                                 (assoc res key r))))
                           {}
                           (:map model))]
      (if (coercion/error? known)
        known
        (if (:open? model)
          (merge value known)
          (if (> (count value) (count known))
            (coercion/map->CoercionError
             {:problems [(str "Undefined parameters: " (apply str (interpose ", " (set/difference (set (keys value)) (set (keys known))))))]})
            known))))

    :else (assert false model)))

(defn realm-coercion
  "Returns a reitit coercion based on realms and the given realm formatter."
  ;; TODO: more docstring
  ;; TODO: coercion comes after parsing (json, transit, something else); clarify that relationship.
  [body-format & {string-format :strings}]
  ;; see https://github.com/metosin/reitit/blob/ff99ab3ff929ca1b5fd7446d72d1a6eb07668795/modules/reitit-core/src/reitit/coercion.cljc#L39
  ;; for type/open/keywordize
  (let [string-format (or string-format common/default-string-format)]
    (reify coercion/Coercion
      (-get-name [_this] :active.data.translate)
      (-get-options [_this] nil)
      ;; doesn't support apidocs yet (and maybe it can't)
      (-get-apidocs [_this _specification _data] nil)
      (-get-model-apidocs [_this _specificat _model _options] nil)
      (-compile-model [_this model name]
        ;; model will be a sequence of realms here, or maps for query/path params
        (assert (= 1 (count model)) "TODO: what do multiple models mean?")
        (->> model
             (map (fn [model]
                    (compile-model body-format string-format model name)))
             (first)))

      (-open-model [_this model]
        (if (instance? MapModel model)
          (assoc model :open? true)
          (assert false (str "Cannot open a realm model: " model))))
      (-encode-error [_this error]
        ;; error is the content of coercion/map->CoercionError here
        error)
      (-request-coercer [_this type model]
        ;; model is the result of compile-model
        (case type
          :body (partial convert-to-model model)
          :string (partial convert-to-model model)
          (assert false (str "type: " type))))
      (-response-coercer [_this model]
        ;; model is the result of compile-model here
        (assert (instance? RealmModel model))
        (fn [value format]
          (assert (nil? format) (str "TODO: What does " format "mean?"))
          ((:from model) value))))))

