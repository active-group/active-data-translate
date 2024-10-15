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
   rec-b :- realm/integer])

(def reverse-string (lens/xmap string/reverse string/reverse))

(def inc-lens (lens/xmap inc dec))

(t/deftest record-map-formatter-test
  ;; Note: is the lens instead of the keyword really helpful? That /can/ be done with a special realm;
  ;; what can be done if it's forced to be a keyword, is a faster transformation; and realm of the output?
  (let [rec-fmt (format/record-map-formatters
                 {rec-ab {rec-a :a
                          rec-b (lens/>> :b inc-lens)}})
        fmt (format/format :my-format
                           (format/combine-formatters
                            (format/simple-formatters {realm/string reverse-string
                                                       realm/integer lens/id})
                            rec-fmt))]
    (t/is (= {:a "oof"
              :b 41}
             (core/translate-from rec-ab fmt (rec-ab rec-a "foo"
                                                     rec-b 42))))
    (t/is (= (rec-ab rec-a "foo"
                     rec-b 12)
             (core/translate-to rec-ab fmt {:a "oof"
                                            :b 11})))))

(t/deftest record-map-formatter-from-vector-test
  (let [rec-fmt (format/record-map-formatters
                 {rec-ab [:a :b]})
        fmt (format/format :my-format
                           (format/combine-formatters
                            (format/simple-formatters
                             {realm/string lens/id
                              realm/integer lens/id})
                            rec-fmt))]
    (t/is (= {:a "foo"
              :b 12}
             (core/translate-from rec-ab fmt (rec-ab rec-a "foo"
                                                     rec-b 12))))
    (t/is (= (rec-ab rec-a "foo"
                     rec-b 12)
             (core/translate-to rec-ab fmt {:a "foo"
                                            :b 12})))))
