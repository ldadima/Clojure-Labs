(ns lab2.second)

(defn integral [fun]
  (fn [x]
    (loop [beg 0 step (/ x 10000) acc 0]
      (if (= beg x)
        acc
        (recur (+ beg step) step (+ (* step (/ (+ (fun beg) (fun (+ beg step))) 2)) acc))
        )
      )
    )
  )