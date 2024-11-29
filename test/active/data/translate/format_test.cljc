(ns active.data.translate.format-test
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

(t/deftest format-error-path-test
  (let [fmt (format/format :my-format
                           {realm/string (format/simple (lens/xmap (fn [v] (throw (format/format-error "Don't like this" v)))
                                                                   (fn [v] (throw (format/format-error "Don't like this" v)))))
                            realm/integer (format/simple lens/id)
                            rec-ab (formatter/record-map rec-ab [:a :b])})
        get-error-data
        (fn [thunk]
          (try (thunk)
               ::did-not-fail
               (catch #?(:clj Exception :cljs :default) e
                 (if (format/format-error? e)
                   (-> (ex-data e)
                       (assoc :message (ex-message e))
                       (dissoc :type))
                   (throw e)))))]
    (t/is (= {:irritant "foo"
              :problem "Don't like this"
              :message "Error formatting \"foo\": Don't like this, at record active.data.translate.format-test/rec-ab with fields rec-a from realm string, rec-b from realm integer > string"
              :path [(realm/compile rec-ab) realm/string]}
             (get-error-data #(core/translate-from rec-ab fmt (rec-ab rec-a "foo"
                                                                      rec-b 12)))))
    (t/is (= {:irritant "foo"
              :problem "Don't like this"
              :message "Error formatting \"foo\": Don't like this, at record active.data.translate.format-test/rec-ab with fields rec-a from realm string, rec-b from realm integer > string"
              :path [(realm/compile rec-ab) realm/string]}
             (get-error-data #(core/translate-to rec-ab fmt {:a "foo"
                                                             :b 12}))))))
