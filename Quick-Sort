;; quick-sort : (listof number)  ->  (listof number)
;; to create a list of numbers with the same numbers as
;; alon sorted in ascending order
(define (quick-sort alon)
  (cond
    [(empty? alon) empty]
    [else
     (append 
      (quick-sort (filter (lambda(x) (< x (first alon))) alon))
      (list (first alon))
      (quick-sort (filter (lambda(x) (>= x (first alon))) (rest alon))))]))


(quick-sort '(1 2 4 3 6 1 9 0))

;; general-quick-sort : (X X  ->  bool) (list X)  ->  (list X)
(define (general-quick-sort p? a-list)
  (local (
          (define (quick-sort alon)
            (cond
              [(empty? alon) empty]
              [else (append (quick-sort (smaller-items (rest alon) (first alon))) 
                            (list (first alon)) 
                            (quick-sort (larger-equal-items (rest alon) (first alon))))]))
          
          ;; larger-items : (listof number) number  ->  (listof number)
          ;; to create a list with all those numbers on alon  
          ;; that are larger than threshold
          (define (larger-equal-items alon threshold)
            (cond
              [(empty? alon) empty]
              [else (if (p? (first alon) threshold) 
                        (cons (first alon) (larger-equal-items (rest alon) threshold))
                        (larger-equal-items (rest alon) threshold))]))
          
          ;; smaller-items : (listof number) number  ->  (listof number)
          ;; to create a list with all those numbers on alon  
          ;; that are smaller than threshold
          (define (smaller-items alon threshold)
            (cond
              [(empty? alon) empty]
              [else (if (p? (first alon) threshold)
                        (smaller-items (rest alon) threshold)
                        (cons (first alon) (smaller-items (rest alon) threshold))
                        )])))  
    (quick-sort a-list)))


(define-struct stu (name gpa))

(define class (list
               (make-stu 'a 2.3)
               (make-stu 'b 0.3)
               (make-stu 'c 4)))

(general-quick-sort (lambda (x y) (< (stu-gpa x) (stu-gpa y))) class) 

(general-quick-sort < '(1 2 4 3 6 1 9 0))


(define (random-list l n)
  (cond 
    ((equal? 0 l) empty)
    (else
     (cons
      (random n) (random-list (- l 1) n)))))

(define test-10 (random-list 10 10))

(time (cons? (quick-sort test-10)))
