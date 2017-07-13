(ns z3-clj.core
  (:refer-clojure :exclude [+ = >= int])
  (:import [com.microsoft.z3 Version Context Expr ArithExpr BoolExpr Sort
            Status])
  (:require [clojure.reflect :as r])
  (:use [clojure.pprint :only [pprint print-table]]))

(def ^:dynamic *context* nil)

(defn- third [seqable] (nth seqable 2))

(defn context [& args] 
  (Context. (into {} (partition 2 args))))

(defn- ->string [thing]
  (cond
   (symbol? thing)  (name thing)
   (keyword? thing) (name thing)
   :else            (str thing)))

(defn- interleaved->map [params]
  (into {} (map #(clojure.core/apply vector %) (partition 2 params))))

(defmacro with-context [params & body]
  (let [params (into [] (map ->string params))]
    `(let [new-context# (Context. ~(interleaved->map params))
           result#      (binding [*context* new-context#] ~@body)
           result#      (if (sequential? result#) (doall result#) result#)]
           (.close new-context#)
           result#)))

(defn get-sort [arg]
  (condp clojure.core/= arg
    'int    (.getIntSort    *context*)
    'real   (.getRealSort   *context*)
    'bool   (.getBoolSort   *context*)
    'string (.getStringSort *context*)
    'unknown-sort))

(defn- make-domain [sorts]
  (into-array Sort 
              (if (sequential? sorts)
                (map get-sort sorts)
                [(get-sort sorts)])))

(defn make-func-decl [name domain range]
  (.mkFuncDecl *context* name (make-domain domain) (get-sort range)))

(defn make-decl [name sort]
  (if (sequential? sort)
    (condp clojure.core/= (first sort)
      'func (make-func-decl name (second sort) (third sort)))
    (.mkConst *context* name (get-sort sort))))

(defn flat-vec [arg]
  (into [] (reduce concat arg)))

(defmacro with-decls [decls & body]
  (let [decls 
         (flat-vec (for [[sort name] (partition 2 decls)]
                     [name `(make-decl ~(->string name) '~sort)] ))]
    `(let ~decls ~@body)))

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

(defn declare-list-type [name & types])

;;;; Function Application

(defn apply [func & args]
  (.mkApp *context* func (into-array Expr args)))

;;;; Arithmetic expressions

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


