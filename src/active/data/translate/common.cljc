(ns active.data.translate.common
  (:require [active.data.translate.format :as format]
            [active.data.realm :as realm]
            [active.clojure.lens :as lens]))

(def ^:private uuid-lens
  (lens/xmap (fn [s]
               (parse-uuid s))
             (fn [uuid]
               (assert (uuid? uuid) uuid)
               (str uuid))))

(def ^:private integer-lens
  #?(:cljs (lens/xmap (fn [s]
                        (js/parseInt s 10))
                      (fn [i]
                        (.toString i)))
     :clj (lens/xmap (fn [s]
                       (Integer/parseInt s))
                     (fn [i]
                       (Integer/toString i)))))

(def ^{:doc "Defines a default format for string coercions, used for path and query parameters."}
  default-string-format
  (format/format ::default-string-format
                 ;; TODO: maybe we can support a bit more, and unions, enums. But not everything can be supported (not as much as for bodies)
                 (format/simple-formatters {realm/string lens/id
                                            realm/uuid uuid-lens
                                            realm/integer integer-lens})))
