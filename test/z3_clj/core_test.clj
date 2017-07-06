(ns z3-clj.core-test
  (:require [clojure.test :refer :all]
            [z3-clj.core :as z3]))


#_(println 
    (z3/with-context [:model :true]
             (let [xExp (z3/int "x")
                   yExp (z3/int "y")
                   opt  (z3/optimizer (z3/= (z3/+ xExp yExp) (z3/int 10))
                                   (z3/>= xExp (z3/int 0))
                                   (z3/>= yExp (z3/int 0)))
                   mx   (z3/maximize opt xExp)
                   my   (z3/maximize opt yExp)
                   status (z3/check opt)]
               (if (z3/satisfiable? status)
                 (map #(-> % .getValue .getInt64) [mx my])))))

(deftest opt-test
  (testing "Optimizer"
    (is (= [10 0]
           (z3/with-context [:model :true]
             (let [xExp (z3/int "x")
                   yExp (z3/int "y")
                   opt  (z3/optimizer (z3/= (z3/+ xExp yExp) (z3/int 10))
                                   (z3/>= xExp (z3/int 0))
                                   (z3/>= yExp (z3/int 0)))
                   mx   (z3/maximize opt xExp)
                   my   (z3/maximize opt yExp)
                   status (z3/check opt)]
               (if (z3/satisfiable? status)
                 (map #(-> % .getValue .getInt64) [mx my]))))))))
