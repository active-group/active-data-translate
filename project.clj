(defproject de.active-group/active-data-translate "1.0.0-SNAPSHOT"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.11.0"]
                 [de.active-group/active-data "0.2.2-SNAPSHOT"]
                 [de.active-group/active-clojure "0.43.0"]]

  :plugins [[lein-codox "0.10.7"]]

  ;; run clojurescript tests via
  ;; > npm run test

  :profiles {:cljs-test
             {:source-paths ["src" "test"]
              :dependencies [[thheller/shadow-cljs "2.27.1"]]}}

  :codox {:language :clojure
          :metadata {:doc/format :markdown}
          :themes [:rdash]
          :src-dir-uri "http://github.com/active-group/active-monad/blob/master/"
          :src-linenum-anchor-prefix "L"})
