(defproject mysema/datomic-tools "0.1.3"
  :description "Mysema Datomic tools"
  :url "http://mysema.com"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [slingshot "0.10.3"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [com.datomic/datomic-pro "0.9.5206"
                  :exclusions [joda-time]
                  :scope "provided"]]
  :repositories {"my.datomic.com" {:url "https://my.datomic.com/repo"}}
  :source-paths ["src/clj"])

