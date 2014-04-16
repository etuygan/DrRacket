;; sierpinski : posn posn posn  ->  true
;; to draw a Sierpinski triangle down at a, b, and c,
;; assuming it is large enough
(define (sierpinski a b c)
  (cond
    [(too-small? a b c) true]
    [else 
      (local ((define ab (mid-point a b))
	      (define bc (mid-point b c))
	      (define ca (mid-point a c)))
	(and
	  (draw-triangle a b c)	    
	  (sierpinski a ab ca)
	  (sierpinski b ab bc)
	  (sierpinski c ca bc)
          ))]))

;; too-small? : posn posn posn  ->  bool
(define (too-small? a b c)
  (<= (+ (sqr (- (posn-x a) (posn-x c))) (sqr (- (posn-y a) (posn-y c)))) 10))

;; draw-triangle : posn posn posn  ->  true
(define (draw-triangle a b c)
  (and
   (draw-solid-line a b 'black)
   (draw-solid-line b c 'black)
   (draw-solid-line c a 'black)))


;; mid-point : posn posn  ->  posn
;; to compute the mid-point between a-posn and b-posn
(define (mid-point a-posn b-posn)
  (make-posn
    (mid (posn-x a-posn) (posn-x b-posn))
    (mid (posn-y a-posn) (posn-y b-posn))))

;; mid : number number  ->  number
;; to compute the average of x and y
(define (mid x y)
  (/ (+ x y) 2))

;; Tests:
(define A (make-posn 200 0))
(define B (make-posn 27 300))
(define C (make-posn 373 300))

(start 400 400)
(sierpinski A B C)
(sleep-for-a-while 3)
(stop)
