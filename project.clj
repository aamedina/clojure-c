(defproject clojure-c "0.1.0-SNAPSHOT"
  :description "Clojure C++ REPL"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-master-SNAPSHOT"]
                 [org.clojure/tools.reader "0.8.16"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :main ^:skip-aot clojure-c.main)
