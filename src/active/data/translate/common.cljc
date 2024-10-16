(ns active.data.translate.common
  (:require [active.data.translate.format :as format]
            [active.data.realm :as realm]
            [active.clojure.lens :as lens]))

(def ^:private uuid-lens
  (lens/xmap (fn [s]
               (or (parse-uuid s) s))
             (fn [uuid]
               (assert (uuid? uuid) uuid)
               (str uuid))))

(def ^:private integer-lens
  #?(:cljs (lens/xmap (fn [s]
                        (let [r (js/parseInt s 10)]
                          (if (js/isNaN r)
                            s
                            r)))
                      (fn [i]
                        (.toString i)))
     :clj (lens/xmap (fn [s]
                       (try (Integer/parseInt s)
                            (catch NumberFormatException _e
                              ;; TODO: we should have validating formats, and non-validating formats.
                              ;; ?? (format/runtime-error )
                              s)))
                     (fn [i]
                       (Integer/toString i)))))

(def ^{:doc "Defines a default format for string coercions, used for path and query parameters."}
  default-string-format
  (format/format ::default-string-format
                 ;; TODO: maybe we can support a bit more, and unions, enums. But not everything can be supported (not as much as for bodies)
                 (format/simple-formatters {realm/string lens/id
                                            realm/uuid uuid-lens
                                            realm/integer integer-lens})))
