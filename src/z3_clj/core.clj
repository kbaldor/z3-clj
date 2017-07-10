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

(defn unsatisfiable? [status]
  (clojure.core/= status Status/UNSATISFIABLE))

(defn unknown? [status]
  (clojure.core/= status Status/UNKNOWN))
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

(defn get-sort [arg]
  (cond
   (clojure.core/= arg int) (.getIntSort *context*)
   (clojure.core/= arg real) (.getRealSort *context*)
    :else "unknown sort"))

;; Arithmetic expressions

(defn + [& arithmetic-expressions]
  (.mkAdd *context* (into-array ArithExpr arithmetic-expressions)))

(defn - [& args]
  (println "found" (count args) "arguments")
  (if (clojure.core/= 1 (count args)) 
    (.mkUnaryMinux *context* (first args))
    (.mkSub *context* (into-array ArithExpr args))))

(defn - [& arithmetic-expressions]
  (.mkSub *context* (into-array ArithExpr arithmetic-expressions)))

(defn * [& arithmetic-expressions]
  (.mkMul *context* (into-array ArithExpr arithmetic-expressions)))

(defn / [lhs rhs]
  (.mkDiv *context* lhs rhs))

;; Boolean expressions
(defn = [lhs rhs]
  (.mkEq *context* lhs rhs))

(defn < [lhs rhs]
  (.mkLt *context* lhs rhs))

(defn <= [lhs rhs]
  (.mkLe *context* lhs rhs))

(defn > [lhs rhs]
  (.mkGt *context* lhs rhs))

(defn >= [lhs rhs]
  (.mkGe *context* lhs rhs))

(defn and [& boolean-expressions]
  (.mkAnd *context* (into-array BoolExpr boolean-expressions)))

(defn or [& boolean-expressions]
  (.mkOr *context* (into-array BoolExpr boolean-expressions)))

(defn xor [& boolean-expressions]
  (.mkXor *context* (into-array BoolExpr boolean-expressions)))

(defn not [boolean-expression]
  (.mkNot *context* boolean-expression))

(defn ite [lhs rhs]
  (.mkITE *context* lhs rhs))

(defn iff [lhs rhs]
  (.mkIff *context* lhs rhs))

(defn -> [lhs rhs]
  (.mkImplies *context* lhs rhs))

(defn check-sat [& constraints]
  (let [solver (.mkSolver *context*)
        status (do 
                   (.add solver (into-array BoolExpr constraints))
                   (.check solver))]
    (cond 
      (satisfiable? status)   (.getModel solver)
      (unsatisfiable? status) (vec (.getUnsatCore solver))
      (unknown? status)       (.getUnknownReason solver))))

(defn optimizer [ & constraints]
  (let [opt (.mkOptimize *context*)]
    (.Add opt (into-array BoolExpr constraints))
    opt))

(defn maximize [optimizer arithmetic-expression]
  (.MkMaximize optimizer arithmetic-expression))

(defn check [opt]
  (.Check opt))


