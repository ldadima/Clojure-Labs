(ns lab2.second-test
  (:require [clojure.test :refer :all]))

(use 'lab2.second)

(deftest zero-x
  (is (= 0 ((integral (fn [x] (* x 2)) 1) 0)))
  )

(deftest zero-fun
  (is (= 0 ((integral (fn [x] 0) 1) 10)))
  )

(deftest negative-x
  (is (= 0 ((integral (fn [x] (* x 2)) -1) -1)))
  )

(deftest negative-fun
  (is (= -100 ((integral (fn [x] (* x -2)) 1) 10)))
  )

(deftest test-1
  (is (= 100 ((integral (fn [x] (* x 2)) 1) 10)))
  )

(deftest test-2
  (is (= 335 ((integral (fn [x] (* x x) ) 1) 10)))
  )
