;;;; LINFO2335 — Assignment Project 2 Functional Programming in Scheme
;;;; Author Ishimwe Jean-Paul

;;; Step 1 : Objects as dispatchers returning functions

(display "   ======================== Step 1 ========================")
(newline)
(display "   Objects as dispatchers returning functions")
(newline)

(define (point x y)
  (define (getx) x)

  (define (gety) y)

  (define (setx! a) (set! x a))

  (define (sety! a) (set! y a))

  (define (type) 'point)

  (define (info) (list 'point x y)) ; info using variables values

  (define (info1) (list type getx gety)) ; info using functions

  (define (add p)
    (point (+ ((p 'getx)) x) (+ ((p 'gety)) y))
    )

  (define (errArg) (display "Message not understood")
    (newline))
  
  (define (self p) ; Renaming dispatch into self
    (cond ((eq? p 'getx) getx)
          ((eq? p 'gety) gety)
          ((eq? p 'type) type)
          ((eq? p 'info) info)
          ((eq? p 'info1) info1)
          ((eq? p 'setx!) setx!)
          ((eq? p 'sety!) sety!)
          ((eq? p 'add) add)
          ((null? p) p)
          (else errArg)
          )
    )
  self)

(define p1 (point 1 2))
(define p2 (point 3 4))
((p1 'getx)) ; returns 1
((p1 'gety)) ; returns 2
((p2 'getx)) ; returns 3
((p2 'gety)) ; returns 4

((p1 'info)) ; returns (point 4 6)
((p1 ' foo )) ; should display ”Message not understood” error

((p1 'setx!) 5)
((p1 'getx)) ; returns 5

(define p ((p1 'add) p2)) ; returns a new point p

((p 'info)) ; should display (point 8 6)

(newline)
(display "   ======================== Step 1 DONE ========================")
(newline)