(ns clojure-c.main
  (:gen-class)
  (:require [clojure-c.repl :as repl]
            [clojure-c.exec :as exec]))

(defn -main
  [& args]
  (try
    (apply repl/init args)
    (catch java.io.IOException e)
    (finally
      (flush))))
