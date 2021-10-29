(ns lab3.second-test
  (:require [clojure.test :refer :all]
            [lab3.second-test :refer :all]))

(use 'lab3.second)

(deftest p-filter-test[]
  (let [
        seq (take 20 (generate-seq))
        ex-seq (doall (filter even? seq))
        ]
   (is (= ex-seq (p-filter even? 2 10 seq)))
   (is (= ex-seq (p-filter even? 4 5 seq)))
   (is (= ex-seq (p-filter even? 2 5 seq)))
   )
  )
