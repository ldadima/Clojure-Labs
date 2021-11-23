(ns lab4.core)

(defn expr? [expr]
  (and
    (coll? expr)
    (keyword? (first expr))
    )
  )

(defn c-var? [expr]
  {:pre [(expr? expr)]}
  (= (first expr) ::var)
  )

(defn c-var [name]
  (list ::var name)
  )

(defn c-var-name [expr]
  {:pre [(c-var? expr)]}
  (first (rest expr))
  )

(defn c-const? [expr]
  {:pre [(expr? expr)]}
  (= (first expr) ::const)
  )

(defn c-const [bool]
  {:pre [(boolean? bool)]}
  (list ::const bool)
  )

(defn c-const-bool [expr]
  {:pre [(c-const? expr)]}
  (first (rest expr))
  )

(defn c-not [a]
  {:pre [(expr? a)]}
  (cons ::not a)
  )

(defn c-and [a b]
  {:pre [(and (expr? a) (expr? b))]}
  (cons ::and (concat a b))
  )

(defn c-or [a b]
  {:pre [(and (expr? a) (expr? b))]}
  (cons ::or (concat a b))
  )

(defn c-impl [a b]
  {:pre [(and (expr? a) (expr? b))]}
  (cons ::impl (concat a b))
  )

(defn c-equal [a b]
  {:pre [(and (expr? a) (expr? b))]}
  (cons ::equal (concat a b))
  )

(defn pos-count
  ([expr c] (if (> c 0)
              (let [e1 (first expr)]
                (cond
                  (= e1 ::and) (+ 1 (pos-count (rest expr) (inc c)))
                  (= e1 ::or) (+ 1 (pos-count (rest expr) (inc c)))
                  (= e1 ::not) (+ 1 (pos-count (rest expr) c))
                  (= e1 ::var) (+ 1 (pos-count (rest expr) c))
                  (= e1 ::const) (+ 1 (pos-count (rest expr) c))
                  (= e1 ::impl) (+ 1 (pos-count (rest expr) (inc c)))
                  (= e1 ::equal) (+ 1 (pos-count (rest expr) (inc c)))
                  :else (+ 1 (pos-count (rest expr) (dec c)))
                  )
                )
              0)
   )
  ([expr] (let [e1 (first expr)]
            (cond
              (= e1 ::and) (+ 1 (pos-count (rest expr) 2))
              (= e1 ::or) (+ 1 (pos-count (rest expr) 2))
              (= e1 ::not) (+ 1 (pos-count (rest expr) 1))
              (= e1 ::var) (+ 1 (pos-count (rest expr) 1))
              (= e1 ::const) (+ 1 (pos-count (rest expr) 1))
              (= e1 ::impl) (+ 1 (pos-count (rest expr) 2))
              (= e1 ::equal) (+ 1 (pos-count (rest expr) 2))
              :else 1
              )))
  )

(defn change-to-base-ops [expr]
  (if (empty? expr)
    ()
    (let [first-symbol (first expr) r (rest expr)]
      (cond
        (= ::impl first-symbol) (cons ::or (cons ::not (change-to-base-ops r)))
        (= ::equal first-symbol) (let [pos-l (pos-count r)
                                       left (take pos-l r)
                                       with-right (drop pos-l r)
                                       pos-r (pos-count with-right)
                                       right (take pos-r with-right)
                                       rr (drop pos-r with-right)
                                       ]
                                   (cons ::or (cons ::and (change-to-base-ops
                                                            (concat
                                                              (cons ::not left) (cons ::not right)
                                                              (cons ::and left) right
                                                              rr
                                                              )
                                                            )))
                                   )
        :else (cons first-symbol (change-to-base-ops r))
        )
      )
    )
  )

(defn remove-extra-not [expr]
  (if (> 2 (count expr))
    expr
    (if (and (= (first expr) ::not) (= (first (rest expr)) ::not))
      (remove-extra-not (rest (rest expr)))
      (cons (first expr) (remove-extra-not (rest expr)))
      )
    )
  )

(defn enter-not [expr]
  (if (> 4 (count expr))
    expr
    (let [e1 (first expr)
          r (rest expr)
          e2 (first r)
          rr (rest r)]
      (if (= e1 ::not)
        (cond
          (= e2 ::or) (let [pos (pos-count rr)]
                        (cons ::and (enter-not
                                      (cons ::not (concat (take pos rr) (cons ::not (drop pos rr))))
                                      ))
                        )
          (= e2 ::and) (let [pos (pos-count rr)]
                         (cons ::or (enter-not
                                      (cons ::not (concat (take pos rr) (cons ::not (drop pos rr))))
                                      ))
                         )
          :else (cons e1 (enter-not r))
          )
        (cons e1 (enter-not r))
        )
      )
    )
  )

(defn distribution-apply [expr]
  (if (empty? expr)
    ()
    (let [f (first expr) r (rest expr)]
      (if (= f ::and) ; (a | b) & c = (a & c) | (b & c) or a & (b | c) = (a & b) | (a & c)
        (let [pos-ab (pos-count r) ab (take pos-ab r) without-ab (drop pos-ab r)
              pos-bc (pos-count without-ab) bc (take pos-bc without-ab) rr (drop pos-bc without-ab)]
          (cond
            (= ::or (first ab)) (let [r-ab (rest ab) pos-a (pos-count r-ab)
                                      a (take pos-a r-ab) b (drop pos-a r-ab) c bc
                                      abc (concat (cons ::and (concat a c)) (cons ::and (concat b c)) rr)]
                                  (cons ::or (distribution-apply abc))
                                  )
            (= ::or (first bc)) (let [r-bc (rest bc) pos-b (pos-count r-bc)
                                      b (take pos-b r-bc) c (drop pos-b r-bc) a ab
                                      abc (concat (cons ::and (concat a b)) (cons ::and (concat a c)) rr)]
                                  (cons ::or (distribution-apply abc))
                                  )
            :else (cons f (distribution-apply r))
            )
          )
        (cons f (distribution-apply r))
        )
      )
    )
  )

(defn extra-distribution
  ([expr] (extra-distribution (distribution-apply expr) (count expr)))
  ([expr prev-len]
   (let [cur-len (count expr)]
     (if (= prev-len cur-len)
       expr
       (extra-distribution (distribution-apply expr) cur-len)
       )
     )
   )
  )

(def check-v #{::or ::and ::impl ::equal})

(defn view [expr]
  (if (empty? expr)
    ()
    (let [first-symbol (first expr) r (rest expr)]
      (cond
        (= ::and first-symbol) (let [pos-right (pos-count r) right (take pos-right r) without-right (drop pos-right r)
                                     pos-left (pos-count without-right) left (take pos-left without-right)]
                                 (str "(" (view right) " & " (view left) ")")
                                 )
        (= ::or first-symbol) (let [pos-right (pos-count r) right (take pos-right r) without-right (drop pos-right r)
                                    pos-left (pos-count without-right) left (take pos-left without-right)]
                                (str "(" (view right) " | " (view left) ")")
                                )
        (= ::impl first-symbol) (let [pos-right (pos-count r) right (take pos-right r) without-right (drop pos-right r)
                                      pos-left (pos-count without-right) left (take pos-left without-right)]
                                  (str "(" (view right) " -> " (view left) ")")
                                  )
        (= ::equal first-symbol) (let [pos-right (pos-count r) right (take pos-right r) without-right (drop pos-right r)
                                       pos-left (pos-count without-right) left (take pos-left without-right)]
                                   (str "(" (view right) " <-> " (view left) ")")
                                   )
        (= ::not first-symbol) (let [pos-expression (pos-count r) expression (take pos-expression r)]
                                 (str "!" (view expression))
                                 )
        (c-var? expr) (str (first r))
        (c-const? expr) (str (first r))
        :else (throw (AssertionError. "Wrong input"))
        )
      )
    )
  )

(defn inner-eval [expr]
  (if (empty? expr)
    ()
    (let [first-symbol (first expr) r (rest expr)]
      (cond
        (= ::or first-symbol) (let [pos-right (pos-count r) right (inner-eval (take pos-right r)) without-right (drop pos-right r)
                                    pos-left (pos-count without-right) left (inner-eval (take pos-left without-right))]
                                (cond
                                  (and (c-const? right) (c-const? left)) (c-const (or (c-const-bool right) (c-const-bool left)))
                                  (c-const? right) (if (c-const-bool right) right left)
                                  (c-const? left) (if (c-const-bool left) left right)
                                  (= right left) right
                                  (and (= ::not (first right)) (= (rest right) left)) (c-const true)
                                  (and (= ::not (first left)) (= (rest left) right)) (c-const true)
                                  (= ::and (first left)) (let [pos-lright (pos-count (rest left)) lright (inner-eval (take pos-lright (rest left))) without-lright (drop pos-lright (rest left))
                                                               pos-lleft (pos-count without-lright) lleft (inner-eval (take pos-lleft without-lright))]
                                                           (cond
                                                             (= lright right) lright
                                                             (= lleft right) lleft
                                                             (and (= ::not (first lright)) (= (rest lright) right)) (c-or right lleft)
                                                             (and (= ::not (first lleft)) (= (rest lleft) right)) (c-or right lright)
                                                             :else (c-or right left)
                                                             )
                                                           )
                                  (= ::and (first right)) (let [pos-rright (pos-count (rest right)) rright (inner-eval (take pos-rright (rest right))) without-rright (drop pos-rright (rest right))
                                                                pos-rleft (pos-count without-rright) rleft (inner-eval (take pos-rleft without-rright))]
                                                            (cond
                                                              (= rright left) rright
                                                              (= rleft left) rleft
                                                              (and (= ::not (first rright)) (= (rest rright) left)) (c-or left rleft)
                                                              (and (= ::not (first rleft)) (= (rest rleft) left)) (c-or left rright)
                                                              :else (c-or right left)
                                                              )
                                                            )
                                  :else (c-or right left)
                                  )
                                )
        (= ::and first-symbol) (let [pos-right (pos-count r) right (take pos-right r) without-right (drop pos-right r)
                                     pos-left (pos-count without-right) left (take pos-left without-right)]
                                 (cond
                                   (and (c-const? right) (c-const? left)) (c-const (and (c-const-bool right) (c-const-bool left)))
                                   (c-const? right) (if (c-const-bool right) left right)
                                   (c-const? left) (if (c-const-bool left) right left)
                                   (= right left) right
                                   (and (= ::not (first right)) (= (rest right) left)) (c-const false)
                                   (and (= ::not (first left)) (= (rest left) right)) (c-const false)
                                   (= ::or (first left)) (let [pos-lright (pos-count (rest left)) lright (inner-eval (take pos-lright (rest left))) without-lright (drop pos-lright (rest left))
                                                               pos-lleft (pos-count without-lright) lleft (inner-eval (take pos-lleft without-lright))]
                                                           (cond
                                                             (= lright right) lright
                                                             (= lleft right) lleft
                                                             (and (= ::not (first lright)) (= (rest lright) right)) (c-and right lleft)
                                                             (and (= ::not (first lleft)) (= (rest lleft) right)) (c-and right lright)
                                                             :else (c-and right left)
                                                             )
                                                           )
                                   (= ::or (first right)) (let [pos-rright (pos-count (rest right)) rright (inner-eval (take pos-rright (rest right))) without-rright (drop pos-rright (rest right))
                                                                pos-rleft (pos-count without-rright) rleft (inner-eval (take pos-rleft
                                                                                                                             without-rright))]
                                                            (cond
                                                              (= rright left) rright
                                                              (= rleft left) rleft
                                                              (and (= ::not (first rright)) (= (rest rright) left)) (c-and left rleft)
                                                              (and (= ::not (first rleft)) (= (rest rleft) left)) (c-and left rright)
                                                              :else (c-and right left)
                                                              )
                                                            )
                                   :else (c-and right left)
                                   )
                                 )
        (= ::not first-symbol) (let [pos-expression (pos-count r) expression (inner-eval (take pos-expression r))]
                                 (cond
                                   (c-const? expression) (c-const (not (c-const-bool expression)))
                                   :else (c-not expression)
                                   )
                                 )
        (c-var? expr) expr
        (c-const? expr) expr
        :else (throw (AssertionError. "Wrong input"))
        )
      )
    )
  )

(defn to-dnf [expr]
  (->> expr
      (change-to-base-ops)
      (remove-extra-not)
      (inner-eval)
      (enter-not)
      (remove-extra-not)
      (extra-distribution)
      (inner-eval)
      )
  )

(defn init-vars [expr symbols]
  (if (empty? expr)
    ()
    (let [f (first expr) r (rest expr)]
      (cond
        (contains? check-v f) (cons f (init-vars r symbols))
        (c-const? expr) (cons f (cons (first r) (init-vars (rest r) symbols)))
        (c-var? expr) (cons ::const (cons (get symbols (first r)) (init-vars (rest r) symbols)))
        :else (throw (AssertionError. "Wrong input"))
        )
      ))
  )

(defn c-eval [expr]
  (if (empty? expr)
    true
    (let [first-symbol (first expr) r (rest expr)]
      (cond
        (= ::or first-symbol) (or (c-eval r) (c-eval (drop (pos-count r) r)))
        (= ::and first-symbol) (and (c-eval r) (c-eval (drop (pos-count r) r)))
        (= ::not first-symbol) (not (c-eval r))
        (= ::impl first-symbol) (or (not (c-eval r)) (c-eval (drop (pos-count r) r)))
        (= ::equal first-symbol) (let [right (c-eval r) left (c-eval (drop (pos-count r) r))]
                                   (or (and right left) (and (not right) (not left)))
                                   )
        (c-const? expr) (first r)
        :else (throw (AssertionError. "Wrong input"))
        )
      )
    )
  )


(defn my-eval [expr symbols]
  {:pre [(and (expr? expr) (map? symbols))]}
  (c-eval (init-vars expr symbols))
  )

(defn test-dnf [expr]
  (print "before: ")
  (println (view expr))
  (print "after: ")
  (println (view (to-dnf expr)))
  )