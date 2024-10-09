(ns active.data.translate.builtin
  (:require [active.data.translate.lang :as lang]
            [active.data.translate.builtin.transit :as transit]))

(def ^{:doc "Translates to and from the Transit format. All standard realms are supported by default, but translations are unstable with respect to changes of the realms."}
  unstable-transit
  ;; is it actually 'the transit format'; we actually just translate to clojure values, that the transit libraries support by default.
  (lang/language ::unstable-transit
                 transit/unstable-defaults))

(def stable-transit
  (lang/language ::stable-transit
                 transit/stable-defaults))
