(ns lab4.core-test
  (:require [clojure.test :refer :all]
            [lab4.core :refer :all]))

(use 'lab4.core)

(deftest dnf-easy-test
  (let [test-expr (c-and (c-impl (c-var "a") (c-const false)) (c-var "d"))
        res (to-dnf test-expr)
        should "((!a & d) | (false & d))"]
    (is (= should (view res)))
    )
  )

(deftest dnf-complex-test
  (let [test-expr (c-and (c-impl (c-not (c-or (c-var "a") (c-not (c-var "k")))) (c-var "b")) (c-equal (c-var "c") (c-var "d")))
        res (to-dnf test-expr)
        should "((((a & (!c & !d)) | (a & (c & d))) | ((!k & (!c & !d)) | (!k & (c & d)))) | ((b & (!c & !d)) | (b & (c & d))))"]
    (is (= should (view res)))
    )
  )
