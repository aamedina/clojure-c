(ns clojure-c.compiler
  (:refer-clojure :exclude [eval compile load])
  (:require [clojure.tools.reader :as rdr]
            [clojure.tools.reader.reader-types :as rdrs]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure-c.exec :as exec
             :refer [with-io *standard-input* *standard-output* *process*]]))

(defprotocol Expr
  (-eval [form])
  (-emit [form])
  (-emit-literal [form]))

(def ^:dynamic *context* :ctx/statement)
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
(def ^:dynamic *debug-io* nil)

(defn env
  []
  (let [current-env (filter (every-pred (comp :dynamic meta) bound?)
                            (vals (ns-publics *ns*)))]
    (zipmap current-env (map deref current-env))))

(defn with-ctx
  [ctx]
  (assoc (env) #'*context* ctx))

(defn with-locals
  [locals]
  (assoc (env) #'*locals* locals))

(defn emit
  ([expr] (emit expr (env)))
  ([expr env]
   (with-bindings env
     (-emit (macroexpand expr)))))

(defn emits
  [& xs]
  (doseq [x xs
          :when x]
    (cond
      (seq? x) (apply emits x)
      (fn? x) (x)
      :else (print x))))

(defn emitln
  [& xs]
  (apply emits xs)
  (newline))

(defn emit-str
  [expr]
  (with-out-str (emit expr)))

(defn emitf
  [fmt & args]
  (printf fmt args))

(defmacro emit-contextually
  [& body]
  `(do
     (when (identical? *context* :ctx/return)
       (emits "return "))
     ~@body
     (when-not (identical? *context* :ctx/expr)
       (emitln ";"))))

(defn pr-unimplemented
  [x]
  (.println System/out (format "UNIMPLEMENTED FORM: %s" x)))

(defn emit-do
  [statements]
  (loop [statements statements
         ret nil]
    (if (seq statements)
      (let [statement (first statements)]
        (recur (rest statements) (emit statement)))
      ret)))

(defn emit-def
  ([sym] (emit-def sym nil))
  ([sym init]
   (emit-contextually
     (emitf "auto %s = %s" (munge sym) (emit init (with-ctx :ctx/expr))))))

(defn emit-if
  ([test then]
   (emitf "(bool(%s)) ? (%s) : NULL"
           (emit test (with-ctx :ctx/expr))
           (emit then (with-ctx :ctx/expr))))
  ([test then else]
   (emitf "(bool(%s)) ? (%s) : (%s)"
           (emit test (with-ctx :ctx/expr))
           (emit then (with-ctx :ctx/expr))
           (emit else (with-ctx :ctx/expr)))))

(defn emit-let
  [[& bindings] exprs]
  (pr-unimplemented 'let))

(defn emit-var
  [sym]
  (pr-unimplemented 'var))

(defn emit-fn
  ([sigs]
   (pr-unimplemented 'fn*))
  ([params exprs]
   (pr-unimplemented 'fn*))
  ([sym params exprs]
   (pr-unimplemented 'fn*)))

(defn emit-loop
  [bindings exprs]
  (pr-unimplemented 'loop*))

(defn emit-recur
  [bindings exprs]
  (pr-unimplemented 'recur))

(defn emit-throw
  [expr]
  (pr-unimplemented 'throw))

(defn emit-catch
  [exception binding exprs]
  (pr-unimplemented 'catch))

(defn emit-finally
  [exprs]
  (pr-unimplemented 'finally))

(defn emit-try
  ([exprs] (pr-unimplemented 'try))
  ([expr catches] (pr-unimplemented 'try))
  ([expr catches finally] (pr-unimplemented 'try)))

(defn emit-case
  [e clauses]
  (pr-unimplemented 'case*))

(defn emit-reify
  [interfaces & impls]
  (pr-unimplemented 'reify*))

(defn emit-letfn
  [[& bindings] exprs]
  (pr-unimplemented 'letfn*))

(defn emit-import
  [lib]
  (emitf "#include <%s>" (.replace (name lib) "." "/"))
  nil)

(defn emit-new
  [class-name args]
  (pr-unimplemented 'new))

(defn emit-delete
  [instance]
  (pr-unimplemented 'delete))

(defn emit-deftype
  [type-name class-name fields interfaces impls]
  (pr-unimplemented 'deftype*))

(defn emit-set!
  [place expr]
  (pr-unimplemented 'set!))

(defn emit-dot
  [expr args]
  (pr-unimplemented 'dot))

(defn emit-invoke
  [[op & args]]
  )

(defn emit-binop
  [op [x y & more]]
  )

(defn emit-seq
  [expr]
  (match (vec expr)
    ['quote form] (-emit-literal form)
    ['do & statements] (emit-do statements)
    ['def sym] (emit-def sym)
    ['def sym init] (emit-def sym init)
    ['if test then] (emit-if test then)
    ['if test then else] (emit-if test then else)
    ['let* bindings & exprs] (emit-let bindings exprs)
    ['var sym] (emit-var sym)
    ['fn* (sym :guard symbol?) (params :guard vector?) & exprs]
    (emit-fn sym params exprs)
    ['fn* (params :guard vector?) & exprs] (emit-fn params exprs)
    ['fn* & sigs] (emit-fn sigs)
    ['loop* bindings & exprs] (emit-loop bindings exprs)
    ['recur & exprs] (emit-recur exprs)
    ['throw expr] (emit-throw expr)
    ['try & exprs] (emit-try exprs)
    ['case* e & clauses] (emit-case e clauses)
    ['reify* interfaces & impls] (emit-reify interfaces impls)
    ['letfn* bindings & exprs] (emit-letfn bindings exprs)
    ['clojure.core/import* lib] (emit-import lib)
    ['new class-name & args] (emit-new class-name args)
    ['delete instance] (emit-delete instance)
    ['deftype* type-name class-name fields :implements interfaces & impls]
    (emit-deftype type-name class-name fields interfaces impls)
    ['set! place expr] (emit-set! place expr)
    ['. expr & args] (emit-dot expr args)
    [(:or '+ '- '/ '*) & args] (emit-binop (first expr) args)
    :else (emit-invoke expr)))

(extend-protocol Expr
  nil
  (-eval [form] nil)
  (-emit [form] (-emit-literal form))
  (-emit-literal [form]
    (print "NULL"))
  Boolean
  (-eval [form] form)
  (-emit [form] (-emit-literal form))
  (-emit-literal [form] (emits (if (true? form) "true" "false")))
  Number
  (-eval [form] form)
  (-emit [form] (-emit-literal form))
  (-emit-literal [form]
    (emits (cond
             (integer? form) (long form)
             (float? form) (double form)
             (ratio? form) (double form))))
  String
  (-eval [form] form)
  (-emit [form] (-emit-literal form))
  (-emit-literal [form] (emits (pr-str form)))
  clojure.lang.Keyword
  (-eval [form] form)
  (-emit [form]
    (pr-unimplemented clojure.lang.Keyword))
  (-emit-literal [form]
    (pr-unimplemented clojure.lang.Keyword))
  clojure.lang.Symbol
  (-eval [form] (throw (UnsupportedOperationException.)))
  (-emit [form]
    (if-let [ns (.getNamespace form)]
      (emits (munge ns) "::" (munge (.getName form)))
      (emits (munge form))))
  (-emit-literal [form]
    (pr-unimplemented clojure.lang.Symbol))
  clojure.lang.ISeq
  (-eval [form] (throw (UnsupportedOperationException.)))
  (-emit [form]
    (emit-seq form))
  (-emit-literal [form]
    (pr-unimplemented clojure.lang.ISeq))
  Object
  (-eval [form] (throw (UnsupportedOperationException.)))
  (-emit [form]
    (pr-unimplemented (.getName (class form))))
  (-emit-literal [form]
    (pr-unimplemented (.getName (class form)))))

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
  (println "COMPILE FILE UNIMPLEMENTED"))

(defn write
  [& more]
  (let [output (str/join \space more)]
    (when exec/*remote-eval*
      (.write *standard-output* output))
    output))

(defn writeln
  [& more]
  (let [output (str/join \space more)]
    (when *debug-io*
      (println "sending: " output))
    (when exec/*remote-eval*
      (.write *standard-output* output)
      (.newLine *standard-output*)
      (when *flush-on-newline*
        (.flush *standard-output*)))
    output))

(defn writef
  [fmt & args]
  (writeln (apply format fmt args)))

(defn eval
  ([form] (eval form (env)))
  ([form env]
   (with-bindings env
     (let [mform (macroexpand form)]
       (when-let [compiled-form (-eval mform)]
         (if exec/*remote-eval*
           (with-out-str
             (when-let [ch (.read *standard-input*)]
               (when (pos? ch)
                 (print (char ch))
                 (while (.ready *standard-input*)
                   (println (.readLine *standard-input*))))))
           compiled-form))))))

(defmacro eval-contextually
  [& body]
  `(do
     (when (identical? *context* :ctx/return)
       (write "return "))
     ~@body
     (when-not (identical? *context* :ctx/expr)
       (writeln ";"))))

(defn eval-do
  [statements]
  (loop [statements statements
         ret nil]
    (if (seq statements)
      (let [statement (first statements)]
        (recur (rest statements) (eval statement)))
      ret)))

(defn eval-def
  ([sym] (eval-def sym nil))
  ([sym init]
   #_(eval-contextually
     (writef "auto %s = %s" (munge sym) (eval init (with-ctx :ctx/expr))))))

(defn eval-if
  ([test then]
   (writef "(bool(%s)) ? (%s) : NULL"
           (eval test (with-ctx :ctx/expr))
           (eval then (with-ctx :ctx/expr))))
  ([test then else]
   (writef "(bool(%s)) ? (%s) : (%s)"
           (eval test (with-ctx :ctx/expr))
           (eval then (with-ctx :ctx/expr))
           (eval else (with-ctx :ctx/expr)))))

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
    ['quote form] form
    ['do & statements] (eval-do statements)
    ['def sym] (eval-def sym)
    ['def sym init] (eval-def sym init)
    ['if test then] (eval-if test then)
    ['if test then else] (eval-if test then else)
    ['let* bindings & exprs] (eval-let bindings exprs)
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
    :else (not-empty (writeln (emit-str (list 'fn* [] form))))))

(def ^:dynamic *load-pathname* nil)
(def ^:dynamic *load-verbose* nil)

(defn load
  [file]
  (binding [*load-pathname* file]
    (doseq [form (forms file)]
      (eval form))))
