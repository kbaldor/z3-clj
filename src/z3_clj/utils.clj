(ns z3-clj.utils)

(defn third [seqable] (nth seqable 2))

(defn ->string [thing]
  (cond
   (symbol? thing)  (name thing)
   (keyword? thing) (name thing)
   :else            (str thing)))

(defn interleaved->map [params]
  (into {} (map #(clojure.core/apply vector %) (partition 2 params))))


