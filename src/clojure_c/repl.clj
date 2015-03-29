(ns clojure-c.repl
  (:refer-clojure :exclude [eval load])
  (:require [clojure-c.compiler :as c]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure-c.exec :as exec
             :refer [with-io *standard-input* *standard-output* *process*]]))

(def ^:dynamic *load-pathname* nil)
(def ^:dynamic *load-verbose* nil)

(defn write
  [& more]
  (let [output (str/join \space more)]
    (when exec/*remote-eval*
      (.write *standard-output* output))
    output))

(defn writeln
  [& more]
  (let [output (str/join \space more)]
    (println "sending: " output)
    (when exec/*remote-eval*
      (.write *standard-output* output)
      (.newLine *standard-output*)
      (when *flush-on-newline*
        (.flush *standard-output*)))
    output))

(defn writef
  [fmt & args]
  (writeln (apply format fmt args)))

(defprotocol Eval
  (-eval [form])
  (-eval-literal [form]))

(defn eval
  ([form] (eval form true))
  ([form top-level?]
   (let [mform (macroexpand form)]
     (when-let [compiled-form (-eval mform)]
       (if exec/*remote-eval*
         (with-out-str
           (when-let [ch (.read *standard-input*)]
             (when (pos? ch)
               (print (char ch))
               (while (.ready *standard-input*)
                 (println (.readLine *standard-input*))))))
         compiled-form)))))

(defn pr-unimplemented
  [x]
  (printf "UNIMPLEMENTED FORM: %s\n" x))

(defn eval-do
  [statements]
  (loop [statements statements
         ret nil]
    (if (seq statements)
      (let [statement (first statements)]
        (recur (rest statements) (eval statement)))
      ret)))

(defn eval-def
  ([sym]
   (writef "auto %s = %s;" (munge sym) "NULL"))
  ([sym init]
   (writef "auto %s = %s;" (munge sym) (eval init false))))

(defn eval-if
  ([test then]
   (writef "(bool(%s)) ? (%s) : NULL" (eval test false) (eval then false)))
  ([test then else]
   (writef "(bool(%s)) ? (%s) : (%s)"
           (eval test false)
           (eval then false)
           (eval else false))))

(defn eval-let
  [[& bindings] exprs]
  (pr-unimplemented 'let))

(defn eval-var
  [sym]
  (pr-unimplemented 'var))

(defn eval-fn
  ([sigs]
   (pr-unimplemented 'fn*))
  ([params exprs]
   (pr-unimplemented 'fn*))
  ([sym params exprs]
   (pr-unimplemented 'fn*)))

(defn eval-loop
  [bindings exprs]
  (pr-unimplemented 'loop*))

(defn eval-recur
  [bindings exprs]
  (pr-unimplemented 'recur))

(defn eval-throw
  [expr]
  (pr-unimplemented 'throw))

(defn eval-catch
  [exception binding exprs]
  (pr-unimplemented 'catch))

(defn eval-finally
  [exprs]
  (pr-unimplemented 'finally))

(defn eval-try
  ([exprs] (pr-unimplemented 'try))
  ([expr catches] (pr-unimplemented 'try))
  ([expr catches finally] (pr-unimplemented 'try)))

(defn eval-case
  [e clauses]
  (pr-unimplemented 'case*))

(defn eval-reify
  [interfaces & impls]
  (pr-unimplemented 'reify*))

(defn eval-letfn
  [[& bindings] exprs]
  (pr-unimplemented 'letfn*))

(defn eval-import
  [lib]
  (writef "#include <%s>" (.replace (name lib) "." "/"))
  nil)

(defn eval-new
  [class-name args]
  (pr-unimplemented 'new))

(defn eval-delete
  [instance]
  (pr-unimplemented 'delete))

(defn eval-deftype
  [type-name class-name fields interfaces impls]
  (pr-unimplemented 'deftype*))

(defn eval-set!
  [place expr]
  (pr-unimplemented 'set!))

(defn eval-dot
  [expr args]
  (pr-unimplemented 'dot))

(defn eval-seq
  [form]
  (match (vec form)
    ['do & statements] (eval-do statements)
    ['def sym] (eval-def sym)
    ['def sym init] (eval-def sym init)
    ['if test then] (eval-if test then)
    ['if test then else] (eval-if test then else)
    ['let* bindings & exprs] (eval-let bindings exprs)
    ['quote form] (-eval-literal form)
    ['var sym] (eval-var sym)
    ['fn* (sym :guard symbol?) (params :guard vector?) & exprs]
    (eval-fn sym params exprs)
    ['fn* (params :guard vector?) & exprs] (eval-fn params exprs)
    ['fn* & sigs] (eval-fn sigs)
    ['loop* bindings & exprs] (eval-loop bindings exprs)
    ['recur & exprs] (eval-recur exprs)
    ['throw expr] (eval-throw expr)
    ['try & exprs] (eval-try exprs)
    ['case* e & clauses] (eval-case e clauses)
    ['reify* interfaces & impls] (eval-reify interfaces impls)
    ['letfn* bindings & exprs] (eval-letfn bindings exprs)
    ['clojure.core/import* lib] (eval-import lib)
    ['new class-name & args] (eval-new class-name args)
    ['delete instance] (eval-delete instance)
    ['deftype* type-name class-name fields :implements interfaces & impls]
    (eval-deftype type-name class-name fields interfaces impls)
    ['set! place expr] (eval-set! place expr)
    ['. expr & args] (eval-dot expr args)
    ['exit] (writeln ".q")
    :else (when-let [compiled (c/compile (list 'fn* [] form))]
            (writeln compiled))))

(extend-protocol Eval
  nil
  (-eval [form] (-eval-literal form))
  (-eval-literal [form] (writeln "NULL"))
  Boolean
  (-eval [form] (-eval-literal form))
  (-eval-literal [form]
    (if (true? form)
      (writeln "true")
      (writeln "false")))
  Number
  (-eval [form] (-eval-literal form))
  (-eval-literal [form]
    (cond
      (integer? form) (writeln (long form))
      (float? form) (writeln (double form))
      (ratio? form) (writeln (double form))))
  String
  (-eval [form] (-eval-literal form))
  (-eval-literal [form] (writeln (pr-str form)))
  clojure.lang.Keyword
  (-eval [form]
    (pr-unimplemented clojure.lang.Keyword))
  (-eval-literal [form]
    (pr-unimplemented clojure.lang.Keyword))
  clojure.lang.Symbol
  (-eval [form]
    (if-let [ns (.getNamespace form)]
      (pr-unimplemented "namespaced symbols")
      (writeln (munge form))))
  (-eval-literal [form]
    (pr-unimplemented clojure.lang.Symbol))
  clojure.lang.ISeq
  (-eval [form]
    (eval-seq form))
  (-eval-literal [form]
    (pr-unimplemented clojure.lang.ISeq))
  Object
  (-eval [form]
    (pr-unimplemented (.getName (class form))))
  (-eval-literal [form]
    (pr-unimplemented (.getName (class form)))))

(defn load
  [file]
  (binding [*load-pathname* file]
    (doseq [form (c/forms file)]
      (eval form false))))

(defn read-eval-print
  []
  (try
    (when-let [line (not-empty (read-line))]
      (binding [*load-verbose* true]
        (if-let [ret (eval (read-string line))]
          (print ret)
          (println nil))))
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
    (binding [*load-verbose* nil]
      (.read *standard-input*)
      (while (.ready *standard-input*)
        (println (.readLine *standard-input*)))
      (load "src/clojure_c/prelude.cljc")
      (while (exec/alive?)
        (print "=> ")
        (flush)
        (read-eval-print)))))
