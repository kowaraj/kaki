(ns sorting.core
  (:require
   [sorting.quicksort]
   ))

(defn sort-quick-sort
  "I don't do a whole lot."
  []
  (println "Quick Sort")
  (let [a '(1 2 3)
        n (count a)]
    (sorting.quicksort/quick-sort :array a :length n)
    (sorting.quicksort/quick-sort :array a :length n :num-of-comparisons true)
    
    ))

(defn test-sorted? [a]
  )
