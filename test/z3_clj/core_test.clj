(ns z3-clj.core-test
  (:require [clojure.test :refer :all]
            [z3-clj.core :refer :all]
            [z3-clj.context :refer :all]))

(println 
    (with-context [:model :true]
      (with-decls [Int x
                   Int y]
        (let
            [opt  (optimizer (EQ (Minus x y) (Real 10))
                             (GT x (Real 100))
                             (LT y (Real 100))
                             (GE x (Real 0))
                             (GE y (Real 0)))
             mx   (maximize opt (Divide x (Real 10)))
             my   (maximize opt y)
             status (check opt)]
          (if (satisfiable? status)
            (map #(clojure.core/-> % .getValue .getInt64) [mx my])
            'unsatisfiable)))))

(with-context [:model :true]
  (with-decls [Int x 
               Int y]
    (println (check-sat (AND (LT x (Int 10))
                             (LT y (Int 5))
                             (Implies (LT x (Int 2))
                                      (GT y (Int 10))))))))

(with-context [:model :true]
  (with-decls [(Func [Int] Int) f
               Int a
               Int b]
    (println (check-sat (GT a (Int 20))
                (GT b a)
                (EQ (Apply f (Int 10)) (Int 1))))))

(with-context [:model :true]
  (with-decls [(Func [Int] Int) f
               Int x
               Int y]
    (println (check-sat
      (EQ (Apply f (Apply f x)) (Apply f x))
      (EQ (Apply f x) y)
      (NOT (EQ x y))))))

(deftest opt-test
  (testing "Optimizer"
    (is (= [10 0]
           (with-context [:model :true]
             (with-decls [Int x
                          Int y]
               (let [opt  (optimizer (EQ (Plus x y) (Int 10))
                                     (GE x (Int 0))
                                     (GE y (Int 0)))
                     mx   (maximize opt x)
                     my   (maximize opt y)
                     status (check opt)]
                 (if (satisfiable? status)
                   (map #(-> % .getValue .getInt64) [mx my])))))))))

