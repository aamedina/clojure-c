(ns clojure-c.repl
  (:refer-clojure :exclude [eval load compile])
  (:require [clojure-c.compiler :refer :all]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure-c.exec :as exec
             :refer [with-io *standard-input* *standard-output* *process*]]))

(defn read-eval-print
  []
  (try
    (when-let [line (not-empty (read-line))]
      (if-let [ret (eval (read-string line))]
        (println ret)
        (println nil)))
    (catch java.io.IOException e)
    (catch Throwable t
      (.printStackTrace t)
      (newline))))

(defn exec-cling
  [& opts]
  (exec/exec "cling"
             "-Xclang" "-std=c++14"
             "-Xclang" "-O0"
             "-Xclang" "-g"
             "-Xclang" "-fstandalone-debug"
             "-Xclang" "-fexceptions"
             "-Xclang" "-ftrapv"
             "-Xclang" "-fno-elide-type"))

(defmacro with-cling
  [opts & body]
  `(with-io (exec-cling ~@opts)
     (let [ret# (do ~@body)]
       (.destroy *process*)
       ret#)))

(defn init
  [& args]
  (with-io (exec-cling)
    (binding [*load-verbose* nil
              *debug-io* true]
      (.read *standard-input*)
      (while (.ready *standard-input*)
        (println (.readLine *standard-input*)))
      (load "src/clojure_c/prelude.cljc")
      (while (exec/alive?)
        (print "=> ")
        (flush)
        (read-eval-print)))))
