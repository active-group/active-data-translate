(defproject de.active-group/active-data-translate "1.0.0-SNAPSHOT"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.11.0"]
                 [de.active-group/active-data "0.2.2-SNAPSHOT"]
                 [de.active-group/active-clojure "0.43.0"]
                 [com.cognitect/transit-cljs "0.8.280" :scope "provided"]
                 [metosin/reitit-core "0.7.2" :scope "provided"]
                 [metosin/reitit-ring "0.7.2" :scope "provided"]
                 [metosin/reitit-spec "0.7.2" :scope "provided"]]

  :plugins [[lein-codox "0.10.7"]]

  :codox {:language :clojure
          :metadata {:doc/format :markdown}
          :themes [:rdash]
          :src-dir-uri "http://github.com/active-group/active-monad/blob/master/"
          :src-linenum-anchor-prefix "L"})
