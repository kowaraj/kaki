(ns sorting.quicksort)

(defn quick-sort
  "Take array a, length n, possible pivot-rules :pivot-first"
  [& {:keys [array length pivot-rule num-of-comparisons]
       :or {pivot-rule :pivot-first
            num-of-comparisons false}
       }]

  (if num-of-comparisons
    (qs-noc-pfirst array length)
    (qs-sort-pfirst array length)
    )
  )
  

(defn qs-choose-pivot [a n & {:keys [pivot-rule] :or {pivot-rule :pivot-first}}]
  (if (= pivot-rule :pivot-first)
    (first a)
    (first (rest a))))
;(qs-choose-pivot [5 1 2 3] 3)

(defn qs-util-swap [a i_ j_]
  (if (= i_ j_)
    a
    (let [[i j] (if (< i_ j_) [i_ j_] [j_ i_])]
      (let [a1 (take i a)
            a2 (take (- j (+ i 1)) (drop (+ i 1) a))
            a3 (drop (+ j 1) a)
            ith (nth a i)
            jth (nth a j)]
        (qs-util-combine [(vec a1) [jth] (vec a2) [ith] (vec a3)])))))
;(qs-util-swap [1 2 3 4 5 6] 4 2)
;(qs-util-swap [1 2 3 4 5 6] 2 4)
;(qs-util-swap [1 2 3 4 5 6 7 8] 3 6)

(defn qs-partition-loop [a pivot i j left right]
  ;;(println "p-loop: " a pivot i j left right)
  (if (> j right)
    (let [
          p (nth a left)
          new-a (qs-util-swap a (- i 1) left)
          new-a1 (vec (take (- i 1) new-a))
          new-a2 (vec (drop i new-a))]
      [new-a1 p new-a2])
    (let [jth (nth a j)]
      (if (< jth pivot)
        (let [new-a (qs-util-swap a i j)
              new-i (+ i 1)
              new-j (+ j 1)]
          (qs-partition-loop new-a pivot new-i new-j left right))
        (let [new-a a
              new-i i
              new-j (+ j 1)]
          (qs-partition-loop new-a pivot new-i new-j left right))))))
  
;(qs-partition-loop [9 5 6 3 7 2 1 4 0] 3 4 4 0 8)
;(qs-partition-loop [9 5 6 3 7 2 1 4 0] 2 6 6 5 8)
  
(defn qs-partition [a-in pi]
  (println "partition: " a-in pi)
  (let [a (qs-util-swap a-in 0 pi) ;put the pivot to 0 position
        pivot (first a)
        i 1
        j 1
        left 0
        right (dec (count a))
        a-partitioned (qs-partition-loop a pivot i j left right) ] ;partitioning
    (println "partitioned = " a-partitioned)
    a-partitioned))

;(qs-partition [9 5 6 3 7 2 1 4 0] 0)
;(qs-partition [2 5 6 3 7 9 1 4 0] 0)
;(qs-partition [7 5 6 3 2 9 1 4 0] 0)
;(qs-partition [3 9 6 7 2 5 1 4 0] 0)

(defn qs-util-combine [vecs]
  ;;(println "combine: " vecs)
  (cond
    (= (count vecs) 1)
    (first vecs)
    (= (count vecs) 2)
    (reduce conj (first vecs) (second vecs))
    :else
    (let [v1 (first vecs)
          vs (rest vecs)]
      (reduce conj v1 (qs-util-combine vs)))))
          

;(def a1 [1 2 3])
;(def a2 [5 6 7])
;(def p 4)
;(qs-util-combine [a1 [p] a2])

(defn qs-choose-pivot-first [a n]
  0)

(defn qs-choose-pivot-different [a n]
  1)

(defn qs-sort-pfirst [a n
                      & {:keys [f-choose-pivot] :or {f-choose-pivot qs-choose-pivot-first}}]
  (println "Sorting (a) by QuickSort, choosing pivot with (f-choose-pivot)")
  (letfn [(qs-sort [a n]
            ;;(println "qs-sort: " a)  
            (cond (= n 0) [] ; base cases
                  (= n 1) [(first a)]
                  (= n 2) (let [x1 (first a) x2 (second a)]
                            (if (< x1 x2) [x1 x2] [x2 x1]))
                  :else ; recursion
                  (let [
                        pi (f-choose-pivot a n)
                        [a1 pivot a2] (qs-partition a pi)
                        a1-sorted (qs-sort a1 (count a1))
                        a2-sorted (qs-sort a2 (count a2))
                        ]
                    ;;(println "sorted: " a1-sorted pivot a2-sorted)
                    (qs-util-combine [a1-sorted [pivot] a2-sorted]))
                  ))]
    (qs-sort a n)))
;; (qs-sort-pfirst [4 9 3 2 8] 5)
;; (qs-sort-pfirst [3 9 6 7 2 5 1 4 0 8] 9 :f-choose-pivot qs-choose-pivot-different)
;; (qs-sort-pfirst [3 9 6 7 2 5 1 4 0 8] 9 :f-choose-pivot qs-choose-pivot-first)
  

(defn qs-noc-pfirst [a n
                     & {:keys [f-choose-pivot] :or {f-choose-pivot qs-choose-pivot-first}}]
  (println "Counting the number of comparisons of QuickSort with pivot-first rule"))
  (letfn [(qs-sort [a n noc]
            ;;(println "qs-sort: " a)  
            (cond (= n 0) [] ; base cases
                  (= n 1) [(first a)]
                  (= n 2) (let [x1 (first a) x2 (second a)]
                            (if (< x1 x2) [x1 x2] [x2 x1]))
                  :else ; recursion
                  (let [
                        pi (f-choose-pivot a n)
                        [a1 pivot a2] (qs-partition a pi)
                        a1-sorted (qs-sort a1 (count a1))
                        a2-sorted (qs-sort a2 (count a2))
                        ]
                    ;;(println "sorted: " a1-sorted pivot a2-sorted)
                    (qs-util-combine [a1-sorted [pivot] a2-sorted]))
                  ))]
    (qs-sort a n 0)))




;; (defn qs-sort-pfirst [a n
;;                       & {:keys [f-choose-pivot] :or {f-choose-pivot qs-choose-pivot-first}}]
;;   (println "Sorting by QuickSort with pivot-first rule")
;;   (letfn [(qs-sort [a n]
;;             (println "qs-sort: " a)
;;             (cond (= n 0) []
;;                   (= n 1) [(first a)]
;;                   (= n 2) (let [x1 (first a) x2 (second a)]
;;                             (if (< x1 x2) [x1 x2] [x2 x1]))
;;                   :else
;;                   (let [pi (f-choose-pivot a n)
;;                         [a1 pivot a2] (qs-partition a pi)
;;                         a1-sorted (qs-sort a1 (count a1))
;;                         a2-sorted (qs-sort a2 (count a2))]
;;                     (println "sorted: " a1-sorted pivot a2-sorted)
;;                     (qs-util-combine [a1-sorted [pivot] a2-sorted]))
;;                   ))]
;;     (qs-sort a n)))
