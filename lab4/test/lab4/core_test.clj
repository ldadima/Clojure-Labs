(ns lab4.core-test
  (:require [clojure.test :refer :all]
            [lab4.core :refer :all]))

(use 'lab4.core)

(deftest dnf-easy1-test
  (let [test-expr (c-and (c-impl (c-var "a") (c-const false)) (c-var "d"))
        res (to-dnf test-expr)
        should "(!a & d)"]
    (is (= should (view res)))
    )
  )

(deftest dnf-easy2-test
  (let [test-expr (c-or (c-var "x") (c-and (c-var "x") (c-var "y")))
        res (to-dnf test-expr)
        should "x"]
    (is (= should (view res)))
    )
  )

(deftest dnf-easy3-test
  (let [test-expr (c-and (c-var "x") (c-or (c-var "x") (c-var "y")))
        res (to-dnf test-expr)
        should "x"]
    (is (= should (view res)))
    )
  )

(deftest dnf-easy4-test
  (let [test-expr (c-and (c-var "x") (c-or (c-not (c-var "x")) (c-var "y")))
        res (to-dnf test-expr)
        should "(x & y)"]
    (is (= should (view res)))
    )
  )

(deftest dnf-easy5-test
  (let [test-expr (c-or (c-var "x") (c-and (c-not (c-var "x")) (c-var "y")))
        res (to-dnf test-expr)
        should "(x | y)"]
    (is (= should (view res)))
    )
  )

(deftest dnf-easy6-test
  (let [test-expr (c-impl (c-equal (c-var "b") (c-var "a")) (c-var "b"))
        res (to-dnf test-expr)
        should "(((b & !a) | (a & !b)) | b)"]
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