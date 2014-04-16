;; sort-list : list-of-numbers  ->  list-of-numbers (sorted)
;; to create a list of numbers with the same numbers as
;; alon sorted in descending order
(define (insert-sort alon)
  (cond
    [(empty? alon) empty]
    [(cons? alon) (insert (first alon) (insert-sort (rest alon)))]))

;; insert : number list-of-numbers (sorted)  ->  list-of-numbers (sorted)
;; to create a list of numbers from n and the numbers on
;; alon that is sorted in descending order; alon is sorted
(define (insert n alon)
  (cond
    [(empty? alon) (cons n empty)]
    [else (cond
            [(<= n (first alon)) (cons n alon)]
            [else (cons (first alon) (insert n (rest alon)))])]))


(insert-sort '(4 3 5 2 1 0 9))
