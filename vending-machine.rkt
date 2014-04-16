;constants

   ;proporties of scene 

(define scene-w 1062)
(define scene-h 402)
(define scene (empty-scene scene-w scene-h))


  ;images for scene


(define coca.)

(define coca-w (image-width coca))
(define  coca-h (image-height coca))


(define cola .)

(define cola-w (image-width cola))
(define cola-h (image-height cola))



;images for ready-state

(define  cokes.)

(define cokes-w (image-width cokes))
(define cokes-h (image-height cokes))

 ;other images

(define coke1 .)(define coke2 .)(define coke3 .)(define coke4 .)
  



  
; functions

; to create new scene "scenery3"  using "place-right.."

;(define (place-right ...)))

 (define  (place-right coca cola scene)
                             (place-image coca  
                                      (- coca-w (/ coca-w 2)) 
                                      (- coca-h (/ coca-h 2))
                             (place-image cola
                                      (- scene-w (/ cola-w 2))  
                                      (- scene-h (/ cola-h 2)) 
                                      
                                      scene)))


 
 
 (define scenery3 (place-right coca cola scene))
 

 ;create "ready" state
 
 (define ready (place-image cokes (+ coca-w (/ cokes-w 2))  (/ scene-h 2) scenery3))
 
 ;define states
 
   (define (state1 x) (and (< 162 x) (< x 288)))
   (define (state2 x) (and (< 288 x) (< x 429)))
   (define (state3 x) (and (< 429 x) (< x 568)))
   (define (state4 x) (and (< 568 x) (< x 710))) 
  

;(to-draw function)
   
;signiture
   ;string --> image 
 
;purpose: to create an image in different states
 
;example
   ; (define (to-draw-handler-function world)...)
   
;conditional template
   
;(define (to-draw-handler-function world) 
;(cond
;[(string=? world "state1") (place-image coke1 230 (/ scene-h 2) scenery3)]
;[(string=? world "state2") (place-image coke2 370 (/ scene-h 2) scenery3)]
;[(string=? world "state3") (place-image coke3 500 (/ scene-h 2) scenery3)]
;[(string=? world "state4") (place-image coke4 650 (/ scene-h 2) scenery3)]
;[else ready])) 


  
;header function
  
  
  (define (to-draw-handler-function world) 
        (cond
              [(string=? world "state1") (place-image coke1 (/ scene-w 2) (/ scene-h 2) scenery3)]
              [(string=? world "state2") (place-image coke2 (/ scene-w 2) (/ scene-h 2) scenery3)]
              [(string=? world "state3") (place-image coke3 (/ scene-w 2) (/ scene-h 2) scenery3)]
              [(string=? world "state4") (place-image coke4 (/ scene-w 2) (/ scene-h 2) scenery3)]
              [else ready]))
    

;(on-key function)
  
;   "p" puts the machine into state1,
;   "c" puts the machine into state2,
;   "z" puts the machine into state3,
;   "d" puts the machine into state4

  
;signiture

  ;Allkeys
;is string which is keyevent
  
  ;Allkeys --> string 

;purpose: to determine next state state of images with keyEvent

;example
    ;(define (on-key-handler-function world key-event)..)

;conditional template
  
;(define (on-key-handler-function world key-event)
;(cond
;[(and (string=? "state1" world) (key=? "p" key-event)) "state1"]
;[(and (string=? "state2" world) (key=? "c" key-event)) "state2"]
;[(and (string=? "state3" world) (key=? "z" key-event)) "state3"]
;[(and (string=? "state4" world) (key=? "d" key-event)) "state4"]
;[(key=? "\r" key-event) "ready"]
;[else world]))
  

;to control function
  
  
      (check-expect (on-key-handler-function "ready" "0") "ready")
      (check-expect (on-key-handler-function "state1" "1") "state1")
      (check-expect (on-key-handler-function "state2" "2") "state2")
      (check-expect (on-key-handler-function "state3" "3") "state3")
      (check-expect (on-key-handler-function "state4" "4") "state4")
  

;header function
      
      (define (on-key-handler-function world key-event)
    (cond
      
      [(and (string=? "ready" world) (key=? "p" key-event)) "state1"]
      [(and (string=? "ready" world) (key=? "c" key-event)) "state2"]
      [(and (string=? "ready" world) (key=? "z" key-event)) "state3"]
      [(and (string=? "ready" world) (key=? "d" key-event)) "state4"]
      [(key=? "\r" key-event) "ready"]
      [else world]))
    

;(on-mouse function)
 
;signiture
    ;string integer integer MouseEvent(string) --> string

;purpose: to determine next state of images with MouseEvent 

;example
    ;(define (on-mouse-handler-function world x y me)...)
      
      
      
;conditional template
      
;(define (on-mouse-handler-function world x y me)
;(cond
;[(and (string=? world "ready") (state1 x) (mouse=? me "button-down")) "state1"]
;[(and (string=? world "ready") (state2 x) (mouse=? me "button-down")) "state2"]
;[(and (string=? world "ready") (state3 x) (mouse=? me "button-down")) "state3"]
;[(and (string=? world "ready") (state4 x) (mouse=? me "button-down")) "state4"]
;[else world]))      
      
      

;header function
      
      (define (on-mouse-handler-function world x y me)
      (cond
        [(and (string=? world "ready") (state1 x) (mouse=? me "button-down")) "state1"]
        [(and (string=? world "ready") (state2 x) (mouse=? me "button-down")) "state2"]
        [(and (string=? world "ready") (state3 x) (mouse=? me "button-down")) "state3"]
        [(and (string=? world "ready") (state4 x) (mouse=? me "button-down")) "state4"]
        [else world]))
  
 
 
  (big-bang 
    "ready"
    (to-draw to-draw-handler-function) 
    (on-mouse on-mouse-handler-function)
    (on-key on-key-handler-function))