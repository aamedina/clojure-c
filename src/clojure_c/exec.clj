(ns clojure-c.exec
  (:require [clojure.java.io :as io])
  (:import [java.util.concurrent TimeUnit]
           [java.io BufferedReader BufferedWriter]
           [java.io InputStream OutputStream PushbackReader]
           [java.io InputStreamReader OutputStreamWriter]))

(def ^:dynamic ^Process *process*)

(def ^:dynamic ^BufferedReader *standard-input*
  (BufferedReader. *in*))

(def ^:dynamic ^BufferedWriter *standard-output*
  (BufferedWriter. *out*))

(def ^:dynamic *remote-eval* false)

(defmacro with-io
  [process & body]
  `(let [^Process process# ~process
         in# (InputStreamReader. (.getInputStream process#))
         out# (OutputStreamWriter. (.getOutputStream process#))
         err# (InputStreamReader. (.getErrorStream process#))]
     (binding [*remote-eval* true
               *process* process#
               *standard-input* (BufferedReader. in#)
               *standard-output* (BufferedWriter. out#)]
       ~@body)))

(defn alive?
  ([] (alive? *process*))
  ([^Process process] (.isAlive process)))

(def ^TimeUnit +milliseconds+ TimeUnit/MILLISECONDS)

(defn wait-for
  ([] (wait-for *process*))
  ([^Process process] (.waitFor process))
  ([^Process process ^long timeout-ms]
   (.waitFor process timeout-ms +milliseconds+)))

(defn exit-value
  ([] (exit-value *process*))
  ([^Process process] (.exitValue process)))

(defn destroy
  ([] (destroy *process*))
  ([^Process process] (.destroy process)))

(defn destroy!
  ([] (destroy! *process*))
  ([^Process process] (.destroyForcibly process)))

(defn ^Process exec
  [& args]
  (when (seq args)
    (let [pb (ProcessBuilder. (map str args))]
      (.redirectErrorStream pb true)
      (.start pb))))
