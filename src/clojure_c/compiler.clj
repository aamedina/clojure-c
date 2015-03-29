(ns clojure-c.compiler
  (:refer-clojure :exclude [eval compile])
  (:require [clojure-c.exec :as exec]
            [clojure.tools.reader :as rdr]
            [clojure.tools.reader.reader-types :as rdrs]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]))

(def ^:dynamic *context* :ctx/statemente)
(def ^:dynamic *locals* nil)
(def ^:dynamic *loop-locals*)
(def ^:dynamic *loop-label*)
(def ^:dynamic *constants*)
(def ^:dynamic *constant-ids*)
(def ^:dynamic *keyword-callsites*)
(def ^:dynamic *protocol-callsites*)
(def ^:dynamic *var-callsites*)
(def ^:dynamic *keywords*)
(def ^:dynamic *vars*)
(def ^:dynamic *in-catch-finally* nil)
(def ^:dynamic *no-recur* nil)

(defn env
  []
  (let [current-env (filter (every-pred (comp :dynamic meta) bound?)
                            (vals (ns-publics *ns*)))]
    (zipmap current-env (map deref current-env))))

(defmulti -compile (fn [[op & args]] op))

(defmethod -compile :default
  [[op & args]]
  (printf "UNIMPLEMENTED COMPILE OP: %s\n" op))

(defn compile
  [expr]
  (-compile expr))

(defn lisp-reader
  [file]
  (clojure.lang.LineNumberingPushbackReader. (io/reader file)))

(defn read-cljc
  [stream eof]
  (clojure.lang.LispReader/read stream false eof false
                                {:read-cond :allow :features #{:cljc}}))

(defn forms
  [file]
  (let [tvec (transient [])
        eof (Object.)]
    (binding [*read-eval* false
              *allow-unresolved-vars* true]
      (with-open [stream (lisp-reader file)]
        (loop [form (read-cljc stream eof)]
          (when-not (identical? form eof)
            (conj! tvec form)
            (recur (read-cljc stream eof))))
        (persistent! tvec)))))

(defn compile-file
  [file]
  (printf "COMPILE FILE UNIMPLEMENTED"))
