(ns lab1.fourth)

(defn add-alf [word alf]
  (map #(cons % word) (filter #(not (= % (first word))) alf))
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