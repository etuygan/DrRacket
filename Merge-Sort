;; MERGE-SORT

;; list-of-numbers  ->  list-of-list-of-single-numbers
(define (split lon) 
  (cond
    [(empty? lon) empty]
    [else (cons (list (first lon)) (split (rest lon)))]))

(split '(1 3 4 9 0))

;; list-of-numbers[sorted] list-of-numbers[sorted]  ->  list-of-numbers[sorted]
(define (merge l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [(< (first l1) (first l2))
     (cons (first l1) (merge (rest l1) l2))]
    [else
     (cons (first l2) (merge l1 (rest l2)))]))

(merge '(3 2 4) '(0 3 9 8 1))


;;  list-of-list-of-sorted-numbers --> list-of-list-of-sorted-numbers
(define (merge-neighbors lon)
  (cond
    [(empty? lon) empty]
    [(empty? (rest lon)) (list (first lon))]
    [else
     (cons (merge (first lon) (second lon)) (merge-neighbors (rest (rest lon))))]))
 


;;  (listof number)  ->  (listof number)[sorted]

(define (merge-sort lon)
  (local
    ((define (merge-sort-inner lon)
       (cond
         [(empty? lon) empty]
         [(empty? (rest lon)) (first lon)]
         [else (merge-sort-inner (merge-neighbors lon))])))
    (merge-sort-inner (split lon))))



(merge-sort '(1 2 4 8 0 3))



;; Some test examples
(define (random-list l n)
  (cond ((equal? 0 l) empty)
        (else
         (cons (random n)(random-list (- l 1) n)))))

(define test-10 (random-list 10 10))
(define test-100 (random-list 100 100))
(define test-10000 (random-list 10000 10000))
(define test-20000 (random-list 20000 20000))
(define test-40000 (random-list 40000 40000))
(define test-80000 (random-list 80000 80000))

"Lists with length: 10"

(time (merge-sort test-10))
(time (merge-sort test-100))
