(ns active.data.translate.builtin-test
  (:require [active.data.translate.builtin :as sut]
            [active.data.translate.core :as core]
            [active.data.realm :as realm]
            [clojure.test :as t]))

;; TODO: clojurescript. and interop between clj and cljs (via transit values)

(defn roundtrip [realm v]
  (core/translate-to sut/stable-transit realm (core/translate-from realm sut/stable-transit v)))

(t/deftest stable-transit-test
  ;; TODO: generator/property based testing would be great?
  (t/testing "positives"
    (t/is (= 1.2 (roundtrip realm/number 1.2)))
    ;; (t/is (= \c (roundtrip realm/char \c)))
    (t/is (= :x (roundtrip realm/keyword :x)))
    (t/is (= 'x (roundtrip realm/symbol 'x)))
    (t/is (= "x" (roundtrip realm/string "x")))
    (t/is (= false (roundtrip realm/boolean false)))
    (t/is (= #uuid "034929d6-aa85-4e66-b88f-c5685fb70fa2" (roundtrip realm/uuid #uuid "034929d6-aa85-4e66-b88f-c5685fb70fa2")))
    (t/is (= 42 (roundtrip realm/integer 42)))
    (t/is (= 1.2 (roundtrip realm/real 1.2)))
    (let [[any-1 value-1] [realm/integer 42]
          [any-2 value-2] [realm/string "foo"]]
      (t/is (= [value-1] (roundtrip (realm/sequence-of any-1) [value-1])))
      (t/is (= #{value-1} (roundtrip (realm/set-of any-1) #{value-1})))
      (t/is (= [value-1 value-2] (roundtrip (realm/tuple any-1 any-2) [value-1 value-2])))
      (t/is (= {value-1 value-2} (roundtrip (realm/map-with-keys {value-1 any-2}) {value-1 value-2})))
      (t/is (= {value-1 value-2} (roundtrip (realm/map-of any-1 any-2) {value-1 value-2})))
      (t/is (= value-1 (roundtrip (realm/delay any-1) value-1)))
      (t/is (= value-1 (roundtrip (realm/named :name any-1) value-1)))
      (t/is (= nil (roundtrip (realm/optional any-1) nil)))
      (t/is (= value-1 (roundtrip (realm/optional any-1) value-1))))))
