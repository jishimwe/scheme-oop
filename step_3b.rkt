;;;; LINFO2335 — Assignment Project 2 Functional Programming in Scheme
;;;; Author Ishimwe Jean-Paul

;;; Step 3b : Introducing inheritance

(display "   ======================== Step 3b ========================")
(newline)
(display "   Introducing inheritance")
(newline)

(define (object)
  (define (super . message)
    'nil)
  
  (define (type) 'object)

  (define (errArg . dump) (display "Message not understood")
    (newline))

  (define (self m)
    (cond ((eq? m 'type) type)
          ((null? m) errArg)
          (else errArg)
          )
    )
  self
  )

(define (point x y)
  (define o (object))
  (define (super . message)
    (cond ((null? message) (o message))
          (else (o (car message)))
          )
    )
  
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
          ((eq? p 'super) (super 'type))
          ((null? p) p)
          (else (super))
          )
    )
  self
  )

(define (color-point x y color)
  (define p (point x y))
  (define (super . message)
    (cond ((null? (car message)) (p 'type))
          (else (p (car message))
                )
          )
    )

  (define (type) 'color-point)

  (define (setcolor! c) (set! color c))

  (define (getcolor) color)

  (define (info) (list (type) ((super 'getx)) ((super 'gety)) (getcolor)))

  (define (self cp)
    (cond ((eq? cp 'setcolor!) setcolor!)
          ((eq? cp 'getcolor) getcolor)
          ((eq? cp 'type) type)
          ((eq? cp 'info) info)
          ((eq? cp 'null) (super 'errArg))
          ((eq? cp 'super) (super))
          (else
           (super cp))
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
               ((eq? ((t 'type)) 'object) #t)
               ((eq? ((t 'type)) 'color-point) #t)
               (else #f)
               )
         )
        (else #f)
        )
  )

(define (object-wrapper obj subObj)
  (cond ((procedure? subObj)
         (cond ((eq? ((obj 'type)) ((subObj 'type))) subObj)
               ((eq? ((obj 'type)) 'point) obj)
               ((eq? ((obj 'type)) 'color-point)
                ((eval ((obj 'type)) (interaction-environment)) 
                ;(color-point
                 ((subObj 'getx))
                 ((subObj 'gety))
                 ((obj 'getcolor)))
                )
               (else
                subObj)
               )
         )
        (else subObj)
        )
  )
  

(define (method-lookup receiver message)
  (receiver message)
  )

(define (send receiver message . args)
  (cond ((null? receiver) errType)
        ((null? message) errArgs)
        ((typeCheck? receiver) ; Check if it's a valid object
         (object-wrapper receiver
                         (apply (method-lookup receiver message) args))
         )
        (else (errType))
        )
  )

(define o (object))
(send o 'type)
(send o 'foo)
(send 'not-object 'type)

  
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

(define cp (color-point 5 6 'red))
(send cp 'type)
(send cp 'info)
(send cp 'getx)

(send cp 'sety! 11)
(send cp 'setcolor! 'blue)
(send cp 'info)

(define cp1 (send cp 'add (color-point 1 2 'green)))
(send cp1 'type)
(send cp1 'getx)
(send cp1 'gety)
(send cp1 'getcolor) ; should display cp color : blue
(send cp1 'info)

(send 'not-a-point 'info) ; should display ”Inappropriate receiver object”

(send p 'foo)  ; should display ”Message not understood”
(send p 'bar 2)  ; should display ”Message not understood”

(display "   ======================== Step 3b DONE ========================")
(newline)
(newline)