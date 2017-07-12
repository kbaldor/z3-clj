(ns z3-clj.core-test
  (:require [clojure.test :refer :all]
            [z3-clj.core :refer :all]))

(println 
    (with-context [:model :true]
      (with-vars [int x
                  int y]
        (let
            [opt  (optimizer (= (- x y) (real 10))
                             (< x (real 100))
                             (< y (real 100))
                             (>= x (real 0))
                             (>= y (real 0)))
             mx   (maximize opt (/ x (real 10)))
             my   (maximize opt y)
             status (check opt)]
          (if (satisfiable? status)
            (map #(clojure.core/-> % .getValue .getInt64) [mx my])
            'unsatisfiable)))))

(with-context [:model :true]
  (with-vars [int x 
              int y]
    (println (check-sat (and (< x (int 10))
                             (< y (int 5))
                             (-> (< x (int 2))
                                 (> y (int 10))))))))

(deftest opt-test
  (testing "Optimizer"
    (is (clojure.core/= [10 0]
           (with-context [:model :true]
             (with-vars [int x
                         int y]
               (let [opt  (optimizer (= (+ x y) (int 10))
                                     (>= x (int 0))
                                     (>= y (int 0)))
                     mx   (maximize opt x)
                     my   (maximize opt y)
                     status (check opt)]
                 (if (satisfiable? status)
                   (map #(clojure.core/-> % .getValue .getInt64) [mx my])))))))))

;;;; Example validation for volume limiting construct

;;; inputs
;;;   upper-limit 
;;;   lower-limit
;;;   increase-volume
;;;   decrease-volume
;;; output
;;;   volume
;;;
;;; parameters
;;;    initial-volume
;;;
;;; helper-functions
;;;    (legal-value lower upper value)
;;;       (and (>= value lower) (<= value upper)) -> (legal-value lower upper value)
;;;
;;; state-variables
;;;    increase-request = (snapshot (+ volume 1) increase-volume)
;;;    decrease-request = (snapshot (- volume 1) decrease-volume)
;;;    change-request   = (or-else increase-request decrease-request)
;;;    legal-change-request = (filter (legal-value lower-limit upper-limit change-request))
;;;    volume = (hold legal-change-request initial-volume)
