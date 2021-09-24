(ns lab1.fourth)

(defn add-alf [word alf]
  (map #(cons % word) (filter #(not (= % (first word))) alf))
  )

(defn add-words [words alf]
  (reduce (fn [acc word] (concat acc (add-alf word alf))) () words)
  )

(defn write-words [n symbols]
  (reduce (fn [words num] (add-words words symbols)) '(()) (range 1 (inc n)))
  )