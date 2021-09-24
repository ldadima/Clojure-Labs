(ns lab2.first)

(defn- square [func beg step]
  (* step (/ (+ (func beg) (func (+ beg step))) 2))
  )

(defn integral [func]
  (let [
        inner-fun (memoize (fn [inner-fun beg end step]
                             (if (>= beg end)
                               0
                               (+ (square func beg step) (inner-fun inner-fun (+ beg step) end step))
                               )
                             )
                           )
        inner-fun (partial inner-fun inner-fun)]
    (fn [x step] (inner-fun 0 x step)))
  )

(defn test-fun [x step]
  (let [fun3x (fn [] (integral (fn [x] (* 3 x))))]
    (time ((fun3x) x step))
    (time ((fun3x) x (/ step 2)))
    (time ((fun3x) x (/ step 4)))
    (time ((fun3x) x step))
    (time ((fun3x) x (/ step 2)))
    (time ((fun3x) x (/ step 4)))
    )
  )