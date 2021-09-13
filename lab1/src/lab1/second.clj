(ns lab1.second)

(defn add-alf [word alf]
  (loop [w word
         a alf
         res '()]
    (if (> (count a) 0)
      (if (some #{(first a)} w)
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
  (if (> n 0)
    (loop [cnt n ss symbols res (add-alf '() ss)]
      (if (> cnt 1)
        (recur (dec cnt) ss (add-words res ss))
        res
        )
      )
    '()
    )
  )
