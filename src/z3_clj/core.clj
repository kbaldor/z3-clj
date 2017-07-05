(ns z3-clj.core
  (:refer-clojure :exclude [+ = >= int])
  (:import [com.microsoft.z3 Version Context ArithExpr BoolExpr
            Status])
  (:require [clojure.reflect :as r])
  (:use [clojure.pprint :only [print-table]]))

(def ^:dynamic *context* nil)

(defn context [& args] 
  (Context. (into {} (partition 2 args))))

(defn satisfiable? [status]
  (clojure.core/= status Status/SATISFIABLE))

;; Arithmetic expressions

(defn int [identifier-or-value]
  (if (string? identifier-or-value)
    (.mkIntConst *context* identifier-or-value)
    (.mkInt      *context* identifier-or-value)))

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


