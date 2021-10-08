(ns lab1.second)

(defn add-alf [word alf]
  (loop [w word
         a alf
         res '()]
    (if (not (empty? a))
      (if (= (first a) (first w))
        (recur w (rest a) res)
        (recur w (rest a) (cons (cons (first a) w) res))
        )
      res
      )
    )
  )

(defn add-words [words alf]
  (loop [ws words a alf res '()]
    (if (empty? ws)
      res
      (recur (rest ws) a (concat (add-alf (first ws) a) res))
      )
    )
  )

(defn write-words [n symbols]
  (loop [cnt n ss symbols res '(())]
    (if (> cnt 0)
      (recur (dec cnt) ss (add-words res ss))
      res
      )
    )
  )
