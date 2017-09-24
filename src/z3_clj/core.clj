(ns z3-clj.core
  (:import [com.microsoft.z3 Version Context Expr ArithExpr BoolExpr Sort
            Status])
  (:require [clojure.reflect :as r]
            [z3-clj.utils :refer :all])
  (:use [clojure.pprint :only [pprint print-table]]
        [z3-clj.context :only [*context*] ]))

(defn get-sort [arg]
  (condp = arg
    'Int    (.getIntSort    *context*)
    'Real   (.getRealSort   *context*)
    'Bool   (.getBoolSort   *context*)
    'String (.getStringSort *context*)
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
    (condp = (first sort)
      'Func (make-func-decl name (second sort) (third sort)))
    (do
      (.mkConst *context* name (get-sort sort)))))

(defn flat-vec [arg]
  (into [] (reduce concat arg)))

(defmacro with-decls [decls & body]
  (let [decls 
         (flat-vec (for [[sort name] (partition 2 decls)]
                     [name `(make-decl ~(->string name) '~sort)] ))]
    `(let ~decls ~@body)))

(defn satisfiable? [status]
  (= status Status/SATISFIABLE))

(defn unsatisfiable? [status]
  (= status Status/UNSATISFIABLE))

(defn unknown? [status]
  (= status Status/UNKNOWN))
;; TODO: Handle the creation of new Sorts

;; constants and values

(defn Bool [identifier-or-value]
  (if (string? identifier-or-value)
    (.mkBoolConst *context* identifier-or-value)
    (.mkBool      *context* identifier-or-value)))

(defn Real [identifier-or-value]
  (if (string? identifier-or-value)
    (.mkRealConst *context* identifier-or-value)
    (.mkReal      *context* identifier-or-value)))

(defn Int 
  ([identifier-or-value]
   (if (string? identifier-or-value)
     (.mkIntConst *context* identifier-or-value)
     (.mkInt      *context* identifier-or-value)))
  ([identifier size]
   (.mkBVConst *context* identifier size)))

(defn declare-list-type [name & types])

;;;; Function Application

(defn Apply [func & args]
  (.mkApp *context* func (into-array Expr args)))

;;;; Arithmetic expressions

(defn Plus [& arithmetic-expressions]
  (.mkAdd *context* (into-array ArithExpr arithmetic-expressions)))

(defn Minus [& args]
  (println "found" (count args) "arguments")
  (if (= 1 (count args)) 
    (.mkUnaryMinus *context* (first args))
    (.mkSub *context* (into-array ArithExpr args))))

(defn Product [& arithmetic-expressions]
  (.mkMul *context* (into-array ArithExpr arithmetic-expressions)))

(defn Divide [lhs rhs]
  (.mkDiv *context* lhs rhs))

;; Boolean expressions
(defn EQ [lhs rhs]
  (.mkEq *context* lhs rhs))

(defn LT [lhs rhs]
  (.mkLt *context* lhs rhs))

(defn LE [lhs rhs]
  (.mkLe *context* lhs rhs))

(defn GT [lhs rhs]
  (.mkGt *context* lhs rhs))

(defn GE [lhs rhs]
  (.mkGe *context* lhs rhs))

(defn AND [& boolean-expressions]
  (.mkAnd *context* (into-array BoolExpr boolean-expressions)))

(defn OR [& boolean-expressions]
  (.mkOr *context* (into-array BoolExpr boolean-expressions)))

(defn XOR [& boolean-expressions]
  (.mkXor *context* (into-array BoolExpr boolean-expressions)))

(defn NOT [boolean-expression]
  (.mkNot *context* boolean-expression))

(defn ITE [lhs rhs]
  (.mkITE *context* lhs rhs))

(defn IFF [lhs rhs]
  (.mkIff *context* lhs rhs))

(defn Implies [lhs rhs]
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


