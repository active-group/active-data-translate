(ns active.data.translate.format-test
  (:require [active.data.translate.format :as format]
            [active.data.translate.core :as core]
            [active.data.realm :as realm]
            [active.data.record :as r]
            [active.clojure.lens :as lens]
            [clojure.test :as t]
            [clojure.string :as string]))

(r/def-record rec-ab
  [rec-a :- realm/string
   rec-b :- realm/string])

(def reverse-string (lens/xmap string/reverse string/reverse))

(t/deftest record-map-formatter-test
  (let [fmt (format/format :my-format
                           (format/combine-formatters
                            (format/simple-formatters
                             {realm/string reverse-string})
                            (format/record-map-formatters
                             {rec-ab {:a rec-a
                                      :b rec-b}})))]
    (t/is (= {:a "oof"
              :b "rab"}
             (core/translate-from rec-ab fmt (rec-ab rec-a "foo"
                                                     rec-b "bar"))))
    (t/is (= (rec-ab rec-a "foo"
                     rec-b "bar")
             (core/translate-to rec-ab fmt {:a "oof"
                                            :b "rab"})))))
