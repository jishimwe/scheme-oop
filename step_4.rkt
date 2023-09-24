;;;; LINFO2335 — Assignment Project 2 Functional Programming in Scheme
;;;; Author Ishimwe Jean-Paul

;;; Step 4 : Dynamic binding of self

(display "   ======================== Step 4 ========================")
(newline)
(display "   Dynamic binding of self")
(newline)

(define (object)
  (define (super . message)
    'nil)
  
  (define (type) 'object)

  (define (errArg . dump) (display "Message not understood")
    (newline))

  (define (set-self! s)
    (set! self s))

  (define (self m)
    (case m
      ('type type)
      ('setself set-self!)
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

  (define (dummy-fun)
    (display "IN POINT")
    (newline))
  
  (define (late-binding-test)
    (send self 'dummy-fun)
    )
  
  (define (getx) x)

  (define (gety) y)

  (define (setx! a) (set! x a))

  (define (sety! a) (set! y a))

  (define (type) 'point)

  (define (info) (list (send self 'type) x y))

  (define (add p)
    (point (+ ((p 'getx)) x) (+ ((p 'gety)) y))
    )

  (define (substract p)
    (point (- ((p 'getx)) x) (- ((p 'gety)) y))
    )

  (define (div d)
    (point (/ x d) (/ y d))
    )

  (define (mul m)
    (point (* x m) (* x m))
    )

  (define (set-self! s)
    (set! self s)
    ((o 'setself) s))
  
  (define (self p) ; Renaming dispatch into self
    (case p
      ('getx getx)
      ('gety gety)
      ('type type)
      ('info info)
      ('setx! setx!)
      ('sety! sety!)
      ('add add)
      ('substract substract)
      ('div div)
      ('mul mul)
      ('errArg (super))
      ('super (super 'type))
      ('test late-binding-test)
      ('dummy-fun dummy-fun)
      ('setself set-self!)
      (else (super))
      )
    )
  self
  )

(define (color-point x y color)
  (define p (point x y))
  
  (define (super . message)
    (cond ((null? (car message)) (p 'type))
          (else (p (car message)))
          )
    )

  (define (dummy-fun)
    (display "IN COLOR-POINT ")
    (newline)
    )
  
  (define (type) 'color-point)

  (define (setcolor! c) (set! color c))

  (define (getcolor) color)

  (define (info) (append ((super 'info)) (list color))) ; info using super calls

  (define (set-self! s)
    (set! self s)
    ((p 'setself) s))

  (define (self cp)
    (case cp
      ('setcolor! setcolor!)
      ('getcolor getcolor)
      ('type type)
      ('info info)
      ('null (super 'errArg))
      ('super (super))
      ('dummy-fun dummy-fun)
      ('setself set-self!)
      (else (super cp))
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
         (case ((t 'type))
           ('point #t)
           ('object #t)
           ('color-point #t)
           (else #f))
         )
        (else #f)
        )
  )

(define (object-wrapper obj subObj)
  (cond ((procedure? subObj)
         (cond ((eq? ((obj 'type)) ((subObj 'type))) subObj)
               ((eq? ((obj 'type)) 'point) obj)
               ((eq? ((obj 'type)) 'color-point)
                (new (eval ((obj 'type)) (interaction-environment))
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

(define (new object . args)
  (cond ((procedure? object)
         (let ((obj (apply object args)))
         ((obj 'setself) obj)
         obj)
         )
        (else (errType))
        )
  )

(define o (new object))
(send o 'type)
(send o 'foo) ; should display "Message not understood"
(send 'not-object 'type) ; should display "Innapropriate receiver object"
(newline)
  
(define p1 (new point 1 2))
(define p2 (new point 3 4))

(send p1 'getx) ; 1
(send p1 'gety) ; 2
(send p2 'getx) ; 3
(send p2 'gety) ; 4

(send p1 'setx! 5)
(send p1 'sety! 5)
(send p1 'info) ; should display (point 5 5)

(define p (send p1 'add p2))
(send p 'info) ; should display (point  8 9)
(newline)

(define cp (new color-point 5 6 'red))
(send cp 'type)
(send cp 'info) ; should display (color-point 5 6 red)
(send cp 'getx)

(send cp 'sety! 11)
(send cp 'setcolor! 'blue)
(send cp 'info) ; should display (color 5 11 blue)

(define cp1 (send cp 'add (color-point 1 2 'green)))
(send cp1 'type)
(send cp1 'getx)
(send cp1 'gety)
(send cp1 'getcolor)
(send cp1 'info) ; should display (color-point 6 13 blue)
(newline)

(send 'not-a-point 'info) ; should display ”Inappropriate receiver object”
(send p 'foo)  ; should display ”Message not understood”
(send p 'bar 2)  ; should display ”Message not understood”

(send p 'test) ; should disply "IN POINT"
(send cp 'test) ; should display "IN COLOR-POINT"

(newline)
(display "   ======================== Step 4 DONE ========================")
(newline)