;;;; LINFO2335 — Assignment Project 2 Functional Programming in Scheme
;;;; Author Ishimwe Jean-Paul

;;; Step 2 : Defining a message sending operator

(display "   ======================== Step 2 ========================")
(newline)
(display "   Defining a message sending operator")
(newline)

(define (point x y)
  (define (getx) x)

  (define (gety) y)

  (define (setx! a) (set! x a))

  (define (sety! a) (set! y a))

  (define (type) 'point)

  (define (info) (list 'point x y))

  (define (info1) (list type getx gety))

  (define (add p)
    (point (+ ((p 'getx)) x) (+ ((p 'gety)) y))
    )

  (define (errArg . dump) (display "Message not understood")
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
          ((eq? p 'errArg) errArg)
          ((null? p) p)
          (else errArg)
          )
    )
  self)

(define (errType) (display "Innapropriate receiver object")
  (newline))

(define (typeCheck? t)
  (cond ((procedure? t)
         (cond ((eq? ((t 'type)) 'point) #t)
               (else #f)
               )
         )
        (else #f)
        )
  )

(define (senderHelper list)
  (cond ((typeCheck? (car list))
         (cond ((null? (cddr list))
                (
                 ((car list) (cadr list))
                 )
                )
               ((null? (cdddr list))
                (
                 ((car list) (cadr list))
                 (caddr list)
                 )
                )
               (else ((car list) 'errArg))
               )
         )
        (else (errType))
        )
  ) 

(define (send . args)
  (cond ((null? args) errArg)
        ((null? (cdr args)) errArg)
        ((typeCheck? (car args))
         (senderHelper args)
         )
        (else (errType))
        )
  )

(define p1 (point 1 2))
(define p2 (point 3 4))

(send p1 'getx) ; 1
(send p1 'gety) ; 2
(send p2 'getx) ; 3
(send p2 'gety) ; 4

(send p1 'setx! 5)
(send p1 'sety! 5)
(send p1 'info) ; should display (point 5 5)

(define p (send p1 'add p2))
(send p 'info) ; should display (point 8 9)

(send 'not-a-point 'info) ; should display ”Inappropriate receiver object”

(send p 'foo)  ; should display ”Message not understood”
(send p 'bar 2)  ; should display ”Message not understood”

(newline)
(display "   ======================== Step 2 DONE ========================")
(newline)