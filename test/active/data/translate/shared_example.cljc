(ns active.data.translate.shared-example
  (:require [active.data.translate.format :as format]
            [active.data.record :as r]
            [active.clojure.lens :as lens]
            [active.data.realm :as realm]))

(r/def-record plus-request
  [req-x :- realm/integer
   req-y :- realm/integer])

(r/def-record plus-response
  [res-value :- realm/integer])

(def my-body-format
  (format/format :my-body-format
                 (format/combine-formatters
                  (format/simple-formatters {realm/integer lens/id})
                  (format/record-map-formatters
                   {plus-request [:x :y]
                    plus-response [:total]}))))

