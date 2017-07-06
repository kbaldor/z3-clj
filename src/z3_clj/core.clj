(ns z3-clj.core
  (:refer-clojure :exclude [+ = >= int])
  (:import [com.microsoft.z3 Version Context ArithExpr BoolExpr
            Status])
  (:require [clojure.reflect :as r])
  (:use [clojure.pprint :only [pprint print-table]]))

(def ^:dynamic *context* nil)

(defn context [& args] 
  (Context. (into {} (partition 2 args))))

(defn- ->string [thing]
  (cond
   (symbol? thing)  (name thing)
   (keyword? thing) (name thing)
   :else            (str thing)))

(defmacro with-context [params & body]
  (let [params (into [] (map ->string params))]
    `(let [new-context# (Context. (into {} (map (partial apply vector) (partition 2 ~params))))
           result#      (binding [*context* new-context#] ~@body)
           result#      (if (sequential? result#) (doall result#) result#)]
           (.close new-context#)
           result#)))

(defn satisfiable? [status]
  (clojure.core/= status Status/SATISFIABLE))

;; TODO: Handle the creation of new Sorts

;; constants and values

(defn bool [identifier-or-value]
  (if (string? identifier-or-value)
    (.mkBoolConst *context* identifier-or-value)
    (.mkBool      *context* identifier-or-value)))

(defn real [identifier-or-value]
  (if (string? identifier-or-value)
    (.mkRealConst *context* identifier-or-value)
    (.mkReal      *context* identifier-or-value)))

(defn int 
  ([identifier-or-value]
   (if (string? identifier-or-value)
     (.mkIntConst *context* identifier-or-value)
     (.mkInt      *context* identifier-or-value)))
  ([identifier size]
   (.mkBVConst *context* identifier size)))

;; Arithmetic expressions

(defn + [& arithmetic-expressions]
  (.mkAdd *context* (into-array ArithExpr arithmetic-expressions)))

;; Boolean expressions
(defn = [lhs rhs]
  (.mkEq *context* lhs rhs))

(defn >= [lhs rhs]
  (.mkGe *context* lhs rhs))

(defn optimizer [ & boolean-expressions]
  (let [opt (.mkOptimize *context*)]
    (.Add opt (into-array BoolExpr boolean-expressions))
    opt))

(defn maximize [optimizer arithmetic-expression]
  (.MkMaximize optimizer arithmetic-expression))

(defn check [opt]
  (.Check opt))


