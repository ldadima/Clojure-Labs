(ns lab1.third)

(defn my-map [fun list]
  (loop [list list acc []]
    (if (empty? list)
      acc
      (recur (rest list) (conj acc (fun (first list))))
      )
    )
  )

;(defn my-map [fun list]
;  (reduce (fn [acc elem] (cons (fun elem) acc)) () list)
;  )

(defn my-filter [test list]
  (loop [list list acc []]
    (if (empty? list)
      acc
      (if (test (first list))
        (recur (rest list) (conj acc (first list) ))
        (recur (rest list) acc)
        )
      )
    )
  )

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
  (reduce (fn [words num] (add-words words symbols)) '([]) (range 1 (inc n)))
  )
