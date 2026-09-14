(ns active.data.translate.core
  (:require [active.data.translate.format :as format]
            [active.data.translate.translator :as translator]))

(def unsupported-exn? format/unsupported-exn?)

(def format-error? translator/format-error?)

(defn external-realm
  "Returns the realm of the external values for the given internal realm, as defined by the given format.
  May throw [[unsupported-exn?]].
  "
  [realm format]
  (let [translator (format/get-translator format realm)]
    (translator/external-realm translator)))

(defn to-extern
  "Returns a unary function that translates values from their internal to their external represenation, as defined by the given format.
  May throw [[unsupported-exn?]]."
  [realm format]
  (let [translator (format/get-translator format realm)]
    (translator/to-extern translator)))

(defn from-extern
  "Returns a unary function that translates values from their external to their internal represenation, as defined by the given format.
  May throw [[unsupported-exn?]].
  The returned function may throw [[format-error?]]."
  [realm format]
  (let [translator (format/get-translator format realm)]
    (translator/from-extern translator)))

(defn ^:no-doc translator-lens [realm format]
  (let [translator (format/get-translator format realm)
        to-extern (translator/to-extern translator)
        from-extern (translator/from-extern translator)]
    (fn
      ([extern]
       (from-extern extern))
      ([_ intern]
       (to-extern intern)))))
