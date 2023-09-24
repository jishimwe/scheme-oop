;;;; LINFO2335 — Assignment Project 2 Functional Programming in Scheme
;;;; Author Ishimwe Jean-Paul

;;; Step 3a : Cleanup

(display "   ======================== Step 3a ========================")
(newline)
(display "   Cleanup")
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

  (define (substract p)
    (point (- ((p 'getx)) x) (- ((p 'gety)) y))
    )

  (define (div d)
    (point (* x d) (* y d))
    )

  (define (mul m)
    (point (* x m) (* x m))
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
          ((eq? p 'substract) substract)
          ((eq? p 'div) div)
          ((eq? p 'mul) mul)
          ((eq? p 'errArg) errArg)
          ((null? p) p)
          (else errArg)
          )
    )
  self
  )

(define (errType . dump) (display "Innapropriate receiver object")
  (newline))

(define (errArgs . dump) (display "Message not understood")
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
               (else ((car list) 'errArgs))
               )
         )
        (else (errType))
        )
  )

(define (method-lookup receiver message)
  (receiver message)
  )

(define (send receiver message . args)
  (cond ((null? receiver) errType)
        ((null? message) errArgs)
        ((typeCheck? receiver)
         (cond ((null? args)
                ((method-lookup receiver message))
                )
               (else 
                (apply (method-lookup receiver message) args)
                )
               )
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
(send p1 'info)

(define p (send p1 'add p2))
(send p 'info)

(send 'not-a-point 'info) ; should display ”Inappropriate receiver object”

(send p 'foo)  ; should display ”Message not understood”
(send p 'bar 2)  ; should display ”Message not understood”

(newline)
(display "   ======================== Step 3a DONE ========================")
(newline)