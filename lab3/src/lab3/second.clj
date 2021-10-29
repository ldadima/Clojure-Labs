(ns lab3.second)

(defn generate-seq []
   (lazy-seq (cons (rand-int 1000) (generate-seq)))
  )

(defn future-partition [size seq]
  (if (empty? seq)
    ()
    (lazy-seq (cons (take size seq) (future-partition size (drop size seq))))
    )
  )

;(defn create-filter-futures [ffun seq]
;  (map (fn[e] (map #(future (doall (filter ffun %))) e)) seq)
;  )
;
;(defn p-filter [ffun threads tsize seq]
;  (mapcat #(mapcat deref %)
;          (create-filter-futures ffun
;               (map #(future-partition tsize %) (future-partition (* threads tsize) seq))))
;  )

(defn create-filter-futures [ffun seq]
  (map #(future (doall (filter ffun %))) seq)
  )

(defn p-filter [ffun threads tsize seq]
  (mapcat #(mapcat deref %)
          (future-partition threads
                            (create-filter-futures ffun (future-partition tsize seq))))
  )

(defn test-fun [threads seq-size tsize]
  (let [test-seq (take seq-size (generate-seq))
        ffun (fn [e] (Thread/sleep 1) (even? e))]
    (time (doall (filter ffun test-seq)))
    (time (doall (p-filter ffun threads tsize test-seq)))
    (time (doall (filter ffun test-seq)))
    (time (doall (p-filter ffun threads tsize test-seq)))
    (time (doall (filter ffun test-seq)))
    (time (doall (p-filter ffun threads tsize test-seq)))
    (println "partitions time (my not-my)")
    (time (doall (create-filter-futures even?
                                        (map #(future-partition tsize %) (future-partition (* threads tsize) test-seq)))))
    (time (doall (map #(partition tsize %) (partition (* threads tsize) test-seq))))
    ()
    )
  )