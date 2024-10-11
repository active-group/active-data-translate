(ns active.data.translate.builtin
  (:require [active.data.translate.format :as format]
            [active.data.translate.builtin.transit :as transit]))

(def ^{:doc "Translates values described by a realm to values usable by transit. The defaults cover most realms."} transit
  ;; Note: use this only when you are ok with the coupling that this introduces.

  ;; Coupling can for example be
  ;; - between producer and consumer code of the transit values, if
  ;;   they are developed independently
  ;; - between past and future versions of the code, if transit values
  ;;   are written to databases, or if producer and consumer can have different
  ;;   versions of the code.
  ;;
  ;; To prevent that, define translators for every realm that you
  ;; expect to change over time, or all of them to be sure. Then make
  ;; those definitions forwards/backwards-compatible to the extend possible or
  ;; needed, or expect different versions of the data.

  (format/format ::transit
                 transit/extended))
