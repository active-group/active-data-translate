(defproject de.active-group/active-data-translate "0.1.0"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.11.0"]
                 [de.active-group/active-data "0.2.2"]
                 [de.active-group/active-clojure "0.43.0"]]

  :plugins [[lein-codox "0.10.8"]]

  ;; run clojurescript tests via
  ;; > npm run test

  :profiles {:cljs-test
             {:source-paths ["src" "test"]
              :dependencies [[thheller/shadow-cljs "2.27.1"]]}}

  :codox {:metadata {:doc/format :markdown}
          :src-dir-uri "http://github.com/active-group/active-monad/blob/master/"
          :src-linenum-anchor-prefix "L"})
