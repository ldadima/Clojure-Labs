(ns lab1.third)

(defn my-map [fun list]
  (if (empty? list)
    ()
    (cons (fun (first list)) (my-map fun (rest list))))
  )

;(defn my-map [fun list]
;  (reduce (fn [acc elem] (cons (fun elem) acc)) () list)
;  )

(defn my-filter [test list]
  (if (empty? list)
    ()
    (if (test (first list))
      (cons (first list) (my-filter test (rest list)))
      (my-filter test (rest list))
      )
    ))

;(defn my-filter [test list]
;  (reduce (fn [acc elem] (if (test elem) (cons elem acc) acc)) () list)
;  )

(defn add-alf [word alf]
  (my-map #(cons % word) (my-filter #(not (= % (first word))) alf))
  )

(defn add-words [words alf]
  (reduce (fn [acc word] (concat acc (add-alf word alf))) () words)
  )

(defn write-words [n symbols]
  (if (> n 0)
    (let [beginWords (add-alf '() symbols)]
      (reduce (fn [words num] (add-words words symbols)) beginWords (range 1 n))
      )
    '(())
    )
  )
