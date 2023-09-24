;;;; LINFO2335 — Assignment Project 2 Functional Programming in Scheme
;;;; Author Ishimwe Jean-Paul

;;; Step 0 : Objects as dispatchers returning values

(display "   ======================== Step 0 ========================")
(newline)
(display "   Objects as dispatchers returning values ")
(newline)

(define (my-cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (display "Argument not 0 or 1"))
          )
    )
  dispatch)

(define (my-car z)
  (z 0))
(define (my-cdr z)
  (z 1))

(define (point0 x y)
  (define (dispatch p)
    (cond ((eq? p 'getx) x)
          ((eq? p 'gety) y)
          ((eq? p 'type) 'point)
          ;((eq? p 'info) (list 'point x y))
          ((eq? p 'info)
           (list (dispatch 'type) (dispatch 'getx) (dispatch 'gety))) ; 'info usging 'getx, 'gety and 'type
          ((eq? p 'add) (point0 (+ x x) (+ y y))) 
          (else (display "Not a valid argument argument or message"))
          )
    )
  dispatch)

(define p (point0 1 3))
(p 'getx) ; should display 1
(p 'gety) ; should display 3
(p 'type) ; should display point
(p 'info) ; should display (point x y)
(p 'foo) ; should display "Not a valid argument argument or message"

(newline)
(display "   ======================== Step 0 DONE ========================")
(newline)