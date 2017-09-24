(ns z3-clj.context
  (:import [com.microsoft.z3 Context])
  (:require [z3-clj.utils :refer :all]))

(def ^:dynamic *context* nil)

(defn context [& args] 
  (Context. (into {} (partition 2 args))))

(defmacro with-context [params & body]
  (let [params (into [] (map ->string params))]
    `(let [new-context# (Context. ~(interleaved->map params))
           result#      (binding [*context* new-context#] ~@body)
           result#      (if (sequential? result#) (doall result#) result#)]
           (.close new-context#)
           result#)))
