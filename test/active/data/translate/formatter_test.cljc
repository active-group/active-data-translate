(ns active.data.translate.formatter-test
  (:require [active.data.translate.formatter :as formatter]
            [active.data.translate.format :as format]
            [active.data.translate.translator :as translator]
            [active.data.translate.core :as core]
            [active.data.realm :as realm]
            [active.data.record :as r #?@(:cljs [:include-macros true])]
            [clojure.test :as t]
            [clojure.string :as string]))

(r/def-record rec-ab
  [rec-a :- realm/string
   rec-b :- realm/integer])

(def reverse-string (translator/translator string/reverse string/reverse realm/string))

(def inc-lens (translator/translator inc dec realm/integer))

(t/deftest record-map-formatter-test
  (let [fmt {realm/string (formatter/simple reverse-string)
             realm/integer (formatter/simple inc-lens)
             rec-ab (formatter/record-map rec-ab
                                          {rec-a :a
                                           rec-b :b})}
        to (core/to-extern rec-ab fmt)
        from (core/from-extern rec-ab fmt)]
    (t/is (= {:a "oof"
              :b 41}
             (to (rec-ab rec-a "foo"
                         rec-b 42))))
    (t/is (= (rec-ab rec-a "foo"
                     rec-b 12)
             (from {:a "oof"
                    :b 11})))))

(t/deftest record-map-formatter-from-vector-test
  (let [fmt {realm/string (formatter/identity realm/string)
             realm/integer (formatter/identity realm/integer)
             rec-ab (formatter/record-map rec-ab [:a :b])}
        to (core/to-extern rec-ab fmt)
        from (core/from-extern rec-ab fmt)]
    (t/is (= {:a "foo"
              :b 12}
             (to (rec-ab rec-a "foo"
                         rec-b 12))))
    (t/is (= (rec-ab rec-a "foo"
                     rec-b 12)
             (from {:a "foo"
                    :b 12})))))

(t/deftest record-map-options-test
  (let [basics {realm/string (formatter/identity realm/string)
                realm/integer (formatter/identity realm/integer)}
        fmt
        (merge basics
               {rec-ab (formatter/record-map rec-ab [:a :b])})

        strict-fmt
        (merge basics
               {rec-ab (formatter/record-map rec-ab [:a :b]
                                             :strict? true)})

        fmt-with-default
        (merge basics
               {rec-ab (formatter/record-map rec-ab [:a :b]
                                             ;; Note that the 42 here is not put through the integer formatter
                                             :defaults {rec-b 42})})]

    (t/testing "ignores extra keys per default"
      (let [from (core/from-extern rec-ab fmt)]
        (t/is (= (rec-ab rec-a "foo"
                         rec-b 12)
                 (from {:a "foo"
                        :b 12
                        :c "test"})))))

    (t/testing "throws on extra keys in strict mode"
      (let [from (core/from-extern rec-ab strict-fmt)]
        (t/is (core/format-error?
               (try (from {:a "foo"
                           :b 12
                           :c "test"})
                    nil
                    (catch #?(:clj Exception :cljs :default) e
                      e))))))

    (t/testing "throws on missing keys per default"
      (let [from (core/from-extern rec-ab fmt)]
        (t/is (core/format-error?
               (try (from {:a "foo"})
                    nil
                    (catch #?(:clj Exception :cljs :default) e
                      e))))))

    (t/testing "uses default for missing keys"
      (let [from (core/from-extern rec-ab fmt-with-default)]
        (t/is (= (rec-ab rec-a "foo"
                         rec-b 42)
                 (from {:a "foo"})))))))

(t/deftest tagged-union-map-test
  (let [union (realm/union realm/string realm/integer)

        fmt {realm/string (formatter/identity realm/string)
             realm/integer (formatter/identity realm/integer)
             union (formatter/tagged-union-map :tag :value {"str" realm/string
                                                            "int" realm/integer})}
        from (core/to-extern union fmt)
        to (core/from-extern union fmt)]

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

        fmt {realm/string (formatter/identity realm/string)
             realm/integer (formatter/identity realm/integer)
             union (formatter/tagged-union-tuple {"str" realm/string
                                                  "int" realm/integer})}
        to-extern (core/to-extern union fmt)
        from-extern (core/from-extern union fmt)]

    (t/is (= ["str" "foo"]
             (to-extern "foo")))
    (t/is (= ["int" 42]
             (to-extern 42)))

    (t/is (= "foo"
             (from-extern ["str" "foo"])))
    (t/is (= 42
             (from-extern ["int" 42])))))

(t/deftest constants-test
  (let [enum (realm/enum :foo)
        fmt {enum (formatter/constants {:foo "foo"})}
        from (core/from-extern enum fmt)
        to (core/to-extern enum fmt)]

    (t/is (= "foo" (to :foo)))
    (t/is (core/format-error? (try (from "bar")
                                   nil
                                   (catch #?(:clj Exception :cljs :default) e
                                     e))))

    (t/is (= :foo (from "foo")))))

(t/deftest identity-test
  (let [fmt (format/combine-formats {realm/integer (formatter/simple inc-lens)}
                                    format/identity)

        int-seq (realm/sequence-of realm/integer)
        from (core/from-extern int-seq fmt)
        to (core/to-extern int-seq fmt)]
    (t/is (= [41] (to [42])))
    (t/is (= [42] (from [41])))))
