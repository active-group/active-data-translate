(ns active.data.translate.formatter-test
  (:require [active.data.translate.format :as format]
            [active.data.translate.formatter :as formatter]
            [active.data.translate.core :as core]
            [active.data.realm :as realm]
            [active.data.record :as r #?@(:cljs [:include-macros true])]
            [active.clojure.lens :as lens]
            [clojure.test :as t]
            [clojure.string :as string]))

(r/def-record rec-ab
  [rec-a :- realm/string
   rec-b :- realm/integer])

(def reverse-string (lens/xmap string/reverse string/reverse))

(def inc-lens (lens/xmap inc dec))

(t/deftest record-map-formatter-test
  (let [fmt (format/format :my-format
                           {realm/string (formatter/simple reverse-string)
                            realm/integer (formatter/simple inc-lens)
                            rec-ab (formatter/record-map rec-ab
                                                         {rec-a :a
                                                          rec-b :b})})
        from (core/translator-from rec-ab fmt)
        to (core/translator-to rec-ab fmt)]
    (t/is (= {:a "oof"
              :b 41}
             (from (rec-ab rec-a "foo"
                           rec-b 42))))
    (t/is (= (rec-ab rec-a "foo"
                     rec-b 12)
             (to {:a "oof"
                  :b 11})))))

(t/deftest record-map-formatter-from-vector-test
  (let [fmt (format/format :my-format
                           {realm/string (formatter/simple lens/id)
                            realm/integer (formatter/simple lens/id)
                            rec-ab (formatter/record-map rec-ab [:a :b])})
        from (core/translator-from rec-ab fmt)
        to (core/translator-to rec-ab fmt)]
    (t/is (= {:a "foo"
              :b 12}
             (from (rec-ab rec-a "foo"
                           rec-b 12))))
    (t/is (= (rec-ab rec-a "foo"
                     rec-b 12)
             (to {:a "foo"
                  :b 12})))))

(t/deftest tagged-union-map-test
  (let [union (realm/union realm/string realm/integer)

        fmt (format/format :my-format
                           {realm/string (formatter/simple lens/id)
                            realm/integer (formatter/simple lens/id)
                            union (formatter/tagged-union-map :tag :value {"str" realm/string
                                                                           "int" realm/integer})})
        from (core/translator-from union fmt)
        to (core/translator-to union fmt)]

    (t/is (= {:tag "str"
              :value "foo"}
             (from "foo")))
    (t/is (= {:tag "int"
              :value 42}
             (from 42)))

    (t/is (= "foo"
             (to {:tag "str"
                  :value "foo"})))
    (t/is (= 42
             (to {:tag "int"
                  :value 42})))))

(t/deftest tagged-union-tuple-test
  (let [union (realm/union realm/string realm/integer)

        fmt (format/format :my-format
                           {realm/string (formatter/simple lens/id)
                            realm/integer (formatter/simple lens/id)
                            union (formatter/tagged-union-tuple {"str" realm/string
                                                                 "int" realm/integer})})
        from (core/translator-from union fmt)
        to (core/translator-to union fmt)]

    (t/is (= ["str" "foo"]
             (from "foo")))
    (t/is (= ["int" 42]
             (from 42)))

    (t/is (= "foo"
             (to ["str" "foo"])))
    (t/is (= 42
             (to ["int" 42])))))
