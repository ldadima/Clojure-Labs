(ns lab2.first-test
  (:require [clojure.test :refer :all]))

(use 'lab2.first)

(deftest zero-x
  (is (= 0 ((integral (fn [x] (* x 2))) 0 1)))
  )

(deftest zero-fun
  (is (= 0 ((integral (fn [x] 0)) 10 1)))
  )

(deftest negative-x
  (is (= 0 ((integral (fn [x] (* x 2))) -1 1)))
  )

(deftest negative-fun
  (is (= -100 ((integral (fn [x] (* x -2))) 10 1)))
  )

(deftest test-1
  (is (= 100 ((integral (fn [x] (* x 2))) 10 1)))
  )

(deftest test-2
  (is (= 335 ((integral (fn [x] (* x x))) 10 1)))
  )
