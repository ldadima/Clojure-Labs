(ns lab1.first)

(defn add-alf [word alf]
  (if (not (empty? alf))
    (if (= (first alf) (first word))
      (add-alf word (rest alf))
      (cons (cons (first alf) word) (add-alf word (rest alf)))
      )
    '()
    )
  )

(defn add-words [words alf]
  (if (empty? words)
    '()
    (concat (add-alf (first words) alf) (add-words (rest words) alf))
    )
  )

(defn write-words [n symbols]
  (if (> n 0)
    (add-words (write-words (dec n) symbols) symbols)
    '(())
    )
  )




