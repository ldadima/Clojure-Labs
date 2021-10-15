(ns lab2.second)

(defn- square [func beg step]
  (* step (/ (+ (func beg) (func (+ beg step))) 2))
  )

(defn integral [func step]
  (let [
        inner-fun (fn integral-fun
                    ([step] (integral-fun 0 step))
                    ([beg step]
                     (cons (square func beg step)
                           (lazy-seq (integral-fun (+ beg step) step))
                           )
                     )
                    )
        inner (reductions + (inner-fun step))
        ]
    (fn [x]
      (if (> x 0) (nth inner (- (/ x step) 1)) 0)
      )
    )
  )

(defn test-fun [x step]
  (let [fun3x (fn [] (integral (fn [x] (* 3 x)) step))]
    (time ((fun3x) x))
    (time ((fun3x) x))
    (time ((fun3x) x))
    (time ((fun3x) x))
    (time ((fun3x) x))
    ;(time ((fun3x) (+ x 50) step))
    ;(time ((fun3x) (+ x 100) step))
    ;(time ((fun3x) x step))
    ;(time ((fun3x) (+ x 50) step))
    ;(time ((fun3x) (+ x 100) step))
    )
  )