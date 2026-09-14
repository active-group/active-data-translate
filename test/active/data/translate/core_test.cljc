(ns active.data.translate.core-test
  (:require [active.data.translate.format :as format]
            [active.data.translate.core :as core]
            [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [active.data.translate.translator :as translator]))

;; internal int, external str
(def int-as-str (translator/translator parse-long str realm/string))

(defn test-format [realm]
  (cond
    (= realm realm/integer) (fn [_resolve] int-as-str)

    (realm-inspection/sequence-of? realm)
    (fn [resolve]
      (let [t (resolve (realm-inspection/sequence-of-realm-realm realm))]
        (translator/translator (fn from-extern [v]
                                 (mapv (translator/from-extern t) v))
                               (fn to-extern [v]
                                 (mapv (translator/to-extern t) v))
                               (realm/sequence-of (translator/external-realm t)))))))

(defn exn [thunk]
  (try (thunk)
       nil
       (catch #?(:clj Exception :cljs :default) e
         e)))

(t/deftest to-extern-test
  (t/is (core/unsupported-exn? (exn #(core/to-extern realm/string format/empty))))

  (t/is (= "42" ((core/to-extern realm/integer test-format)
                 42)))

  (t/is (= ["42" "21"] ((core/to-extern (realm/sequence-of realm/integer) test-format)
                        [42 21]))))

(t/deftest from-extern-test
  (t/is (core/unsupported-exn? (exn #(core/from-extern realm/string format/empty))))

  (t/is (= [42 21] ((core/from-extern (realm/sequence-of realm/integer) test-format)
                    ["42" "21"]))))

