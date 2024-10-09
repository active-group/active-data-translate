(ns active.data.translate.builtin.transit
  (:require [active.data.translate.lang :as lang]
            [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]
            [active.clojure.lens :as lens]
            #?(:cljs [cognitect.transit :as transit])))

(def ^:private transit?
  ;; Note: this can be an expensive call; don't use it too much.
  #?(:cljs (some-fn nil?
                    keyword?
                    string? ;; = char
                    boolean?
                    symbol?
                    #(instance? js/Date %)
                    transit/bigdec? ;; = decimal?
                    transit/bigint?
                    transit/binary?
                    transit/integer?
                    transit/link?
                    transit/quoted?
                    ;; transit/tagged-value?
                    transit/uri?
                    transit/uuid?

                    ;; TODO array, list, set, map, cmap?
                    )
     :clj (some-fn nil?
                   keyword?
                   string?
                   boolean?
                   integer? ;; is that correct?
                   decimal? ;; is that correct?
                   symbol?
                   ;; TODO bigdec, bigint
                   #(instance? java.util.Date %)
                   uri?
                   uuid?
                   char?
                   ;; TODO array, list, set, map, link
                   )))

(def ^:private id lens/id)

(def ^:private transit-uuid
  ;; Note: realm uuid is: clojure.core/uuid? (java.util.UUID) resp. cljs.core/uuid? (cljs.core/UUID)
  ;; transit uuid is: java.util.UUID  resp. com.cognitect.transit in cljs
  #?(:clj id
     :cljs (lens/xmap (fn to-realm [v]
                        ;; v should satisfy (transit/uuid? v)
                        (uuid (str v)))
                      (fn from-realm [v]
                        (transit/uuid (str v))))))

(defn- optional [lens]
  (lens/xmap (fn to-realm [v]
               (when (some? v) (lens/yank v lens)))
             (fn from-realm [v]
               (when (some? v) (lens/shove nil lens v)))))

(defn set-as-seq [seq-lens]
  (lens/xmap (fn to-realm [v]
               (set (lens/yank v seq-lens)))
             (fn from-realm [v]
               (set (lens/shove nil seq-lens v)))))

(def ^:private nil-as-empty-map
  (lens/xmap (fn to-realm [v]
               (if (nil? v)
                 {}
                 v))
             (fn from-realm [v]
               (if (nil? v)
                 {}
                 v))))

(defn stable-defaults [realm recurse]
  ;; TODO: to be called 'stable' these should all detect if a
  ;; translated value (in all yanks) actually (still) conforms to the
  ;; expected format. I don't think they all do yet. Introduce the notion of a runtime error to the lib?

  (cond
    (realm-inspection/builtin-scalar? realm)
    (case (realm-inspection/builtin-scalar-realm-id realm)
      :number id ;; TODO: is this correct?
      :keyword id
      :symbol id
      :string id
      :boolean id
      :uuid transit-uuid
      ;; TODO: can we support :char ? :rational?
      :any (lang/unsupported-exn realm)
      (throw (lang/unsupported-exn realm)))

    (realm-inspection/integer-from-to? realm)
    id

    (realm-inspection/real-range? realm)
    id

    (realm-inspection/optional? realm)
    (let [inner-t (recurse (realm-inspection/optional-realm-realm realm))]
      (optional inner-t))

    (realm-inspection/sequence-of? realm)
    (lens/mapl (recurse (realm-inspection/sequence-of-realm-realm realm)))

    (realm-inspection/set-of? realm)
    (set-as-seq (lens/mapl (recurse (realm-inspection/set-of-realm-realm realm))))

    (realm-inspection/function? realm)
    (throw (lang/unsupported-exn realm))

    (realm-inspection/map-with-keys? realm)
    (lens/pattern (->> (realm-inspection/map-with-keys-realm-map realm)
                       (map (fn [[k value-realm]]
                              (when-not (transit? k)
                                (throw (lang/unsupported-exn k [realm])))
                              [(lens/member k) (lens/>> (lens/member k) (recurse value-realm))]))
                       (into {})))

    (realm-inspection/map-of? realm)
    ;; Note: mapl-kv would get us a nil, instead of an empty map
    (lens/>> nil-as-empty-map
             (lens/mapl-kv (recurse (realm-inspection/map-of-realm-key-realm realm))
                           (recurse (realm-inspection/map-of-realm-value-realm realm))))

    (realm-inspection/tuple? realm)
    (lens/pattern (->> (realm-inspection/tuple-realm-realms realm)
                       (map-indexed (fn [idx realm]
                                      (lens/>> (lens/at-index idx) (recurse realm))))
                       (vec)))

    (realm-inspection/delayed? realm)
    ;; Note: for now we assume, that at the time this translation is fetched, the realm must be resolvable.
    (recurse (force (realm-inspection/delayed-realm-delay realm)))

    (realm-inspection/named? realm)
    (recurse (realm-inspection/named-realm-realm realm))

    (realm-inspection/record? realm)
    (throw (lang/unsupported-exn realm))

    (realm-inspection/union? realm)
    (throw (lang/unsupported-exn realm))

    (realm-inspection/intersection? realm) ;; or allow? 'realm/restricted'
    (throw (lang/unsupported-exn realm))

    ;; TODO: what about map-with-tag?
    (realm-inspection/map-with-tag? realm)
    (throw (lang/unsupported-exn realm))

    :else
    ;; predicate realm
    (throw (lang/unsupported-exn realm))))

(defn- tagged-union-lens [realms recurse]
  ;; represents (union r1 r2) as [0 ->r1] [1 ->r2] etc.
  (let [tags-realms-lenses-map (->> realms
                                    (map-indexed (fn [idx realm]
                                                   [idx [realm (recurse realm)]]))
                                    (into {}))]
    ;; TODO: asserts or exceptions?
    (lens/xmap (fn to-realm [[idx v]]
                 (if-let [[_realm lens] (get tags-realms-lenses-map idx)]
                   (lens/yank v lens)
                   (assert false "Invalid union tag")))
               (let [try-all (->> tags-realms-lenses-map
                                  (map (fn [[idx [realm lens]]]
                                         (fn [value]
                                           (when (realm/contains? realm value)
                                             [idx (lens/shove nil lens value)]))))
                                  (apply some-fn))]
                 (fn from-realm [v]
                   (or (try-all v)
                       (assert false "Value not contained in union.")))))))

(defn- flat-record-lens [realm recurse]
  ;; represents a record as a vector of the values, in the order defined by the record realm.
  (let [realms (map realm-inspection/record-realm-field-realm
                    (realm-inspection/record-realm-fields realm))
        lenses (map recurse realms)
        getters  (map realm-inspection/record-realm-field-getter
                      (realm-inspection/record-realm-fields realm))
        constr (realm-inspection/record-realm-constructor realm)]
    (lens/xmap (fn [edn]
                 (assert (= (count (realm-inspection/record-realm-fields realm))
                            (count edn))
                         (realm-inspection/description realm))
                 (let [vals (map (fn [v lens]
                                   (lens/yank v lens))
                                 edn
                                 lenses)
                       res (apply constr vals)]
                   res))
               (fn [inst]
                 (let [res (let [vals (map (fn [getter]
                                             (getter inst))
                                           getters)
                                 edn (map (fn [v lens]
                                            (lens/shove nil lens v))
                                          vals
                                          lenses)]
                             (vec edn))]
                   res)))))

(defn unstable-defaults [realm recurse]
  (cond
    (realm-inspection/record? realm)
    (flat-record-lens realm recurse)

    (realm-inspection/union? realm)
    (tagged-union-lens (realm-inspection/union-realm-realms realm) recurse)

    ;; Note: because every should conform to all intersected realms, every translation should be able to translate all values.
    ;; So we can just take the first one. (can't be empty)
    (realm-inspection/intersection? realm)
    (recurse (first (realm-inspection/intersection-realm-realms realm)))

    ;; TODO: what about map-with-tag?

    :else
    (stable-defaults realm recurse)))
