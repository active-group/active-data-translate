(ns active.data.translate.builtin-test
  (:require [active.data.translate.builtin :as sut]
            [active.data.translate.core :as core]
            [active.data.realm :as realm]
            [active.data.record :as r]
            [clojure.test :as t]))

;; TODO: clojurescript. and interop between clj and cljs (via transit values)

(defn roundtrip [realm v]
  (core/translate-to realm  sut/transit
                     (core/translate-from realm sut/transit v)))

(t/deftest empty-map-test
  (let [realm (realm/map-of realm/integer realm/integer)
        v {}
        t {}]
    (t/is (= t (core/translate-from realm sut/transit v)))
    (t/is (= v (core/translate-to realm sut/transit t)))))

(t/deftest tuple-test
  (let [realm (realm/tuple realm/string realm/integer)
        v ["foo" 42]
        t ["foo" 42]]
    (t/is (vector? (core/translate-from realm sut/transit v)))
    (t/is (= t (core/translate-from realm sut/transit v)))

    (t/is (vector? (core/translate-to realm sut/transit t)))
    (t/is (= v (core/translate-to realm sut/transit t)))))

(r/def-record rec-ab
  [rec-a :- realm/string
   rec-b :- realm/integer])

(t/deftest transit-roundtrip-test
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
      (t/is (= value-1 (roundtrip (realm/optional any-1) value-1))))
    (t/is (= :foo
             (roundtrip (realm/enum :foo "bar") :foo)))
    (t/is (= (rec-ab rec-a "foo" rec-b 42)
             (roundtrip rec-ab (rec-ab rec-a "foo" rec-b 42))))))
