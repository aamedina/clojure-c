(ns clojure-c.compiler
  (:refer-clojure :exclude [eval compile load])
  (:require [clojure-c.exec :as exec]
            [clojure.tools.reader :as rdr]
            [clojure.tools.reader.reader-types :as rdrs]
            [clojure.core.match :refer [match]]))

(defmulti -compile (fn [[op & args]] op))

(defmethod -compile :default
  [[op & args]]
  (printf "UNIMPLEMENTED COMPILE OP: %s\n"))

(defn compile
  [expr]
  (-compile expr))

(defn compile-file
  [file]
  (printf "COMPILE FILE UNIMPLEMENTED"))

(defn load
  [file]
  (printf "LOAD UNIMPLEMENTED"))
