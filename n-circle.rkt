;; circles: number -> Image
; (define (fcircles n)
;   (cond
;     [(<= n 5) (circle 5 'solid 'red)]
;     [else
;      ...(fcircles (/ n 2))
;      ...(fcircles (/ n 2)) ]))



(define (fcircleso n)
  (cond 
    ((<= n 4) (circle 4 'outline 'black))
    (else
     (local
       ((define sub (fcircleso (/ n 2))))
       (overlay/xy 
        (circle n 'outline 'black)
        (* 3 (/ n -2)) 0 
        (overlay/xy 
         sub
         (* 3 n) 0
         sub))))))

(fcircleso 100)
